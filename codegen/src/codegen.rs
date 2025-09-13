use ecow::EcoString;
use inkwell::FloatPredicate::{OEQ, OGE, OGT, OLE, OLT, ONE};
use inkwell::IntPredicate::{EQ, NE, SGE, SGT, SLE, SLT, UGE, UGT, ULE, ULT};
use inkwell::values::BasicMetadataValueEnum;
use inkwell::{
    AddressSpace, IntPredicate,
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module as InkModule,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
};
use std::{collections::HashMap, ffi::CString};

use crate::hir::{HIRCase, HIRDeclaration, HIRExpr, HIRExprGives, HIRModule, HIRStmt, HIRType};
use ast::ast::{BinaryOp, Constant, InbuiltType, UnaryOp};

pub type CodegenContext = Context;

#[derive(Debug, Clone)]
struct Variable<'ctx> {
    ptr: PointerValue<'ctx>,
    element_type: BasicTypeEnum<'ctx>,
    native_type: HIRType,
}

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    pub module: InkModule<'ctx>,
    builder: Builder<'ctx>,
    #[allow(dead_code)]
    engine: ExecutionEngine<'ctx>,

    strings_counter: u32,

    variables: Vec<HashMap<EcoString, Variable<'ctx>>>,
    functions: HashMap<EcoString, FunctionValue<'ctx>>,
    struct_types: HashMap<EcoString, StructType<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &String) -> Result<Self, EcoString> {
        let module = context.create_module(name);
        let builder = context.create_builder();
        let engine = module
            .create_jit_execution_engine(inkwell::OptimizationLevel::None)
            .map_err(|e| format!("Failed to create JIT engine: {:?}", e))?;

        Ok(Self {
            context,
            module,
            builder,
            engine,
            strings_counter: 0,
            variables: vec![HashMap::new()],
            functions: HashMap::new(),
            struct_types: HashMap::new(),
            current_function: None,
        })
    }

    pub fn dump_ir(&self) {
        self.module.print_to_stderr();
    }

    pub fn compile(&mut self, modules: &[HIRModule]) -> Result<(), String> {
        self.declare_printf();

        for m in modules {
            for decl in &m.decls {
                self.compile_decl(decl)
                    .map_err(|e| format!("Codegen error: {:?}", e))?;
            }
        }

        self.module.verify().expect("Module verification failed");

        Ok(())
    }

    fn compile_decl(&mut self, decl: &HIRDeclaration) -> Result<(), BuilderError> {
        match decl {
            HIRDeclaration::Var { name, ty, init, .. } => {
                let llvm_type = self.to_llvm_basic_type(ty).unwrap();
                let ptr = if let Some(current_fn) = self.current_function {
                    let entry = current_fn.get_first_basic_block().unwrap();
                    let builder_at_entry = self.context.create_builder();
                    builder_at_entry.position_at_end(entry);
                    builder_at_entry.build_alloca(llvm_type, name)?
                } else {
                    let global =
                        self.module
                            .add_global(llvm_type, Some(AddressSpace::default()), name);
                    if let Some(init_expr) = init {
                        let val = self.compile_const_expr(init_expr).unwrap();
                        let casted = self.cast_to_type(val, ty).unwrap();
                        global.set_initializer(&casted);
                    } else {
                        global.set_initializer(&llvm_type.const_zero());
                    }
                    global.as_pointer_value()
                };
                self.insert_var(name.clone(), ptr, llvm_type, ty.clone());
                Ok(())
            }

            HIRDeclaration::Func {
                name,
                ret_ty,
                params,
                body,
                ..
            } => {
                let ret_type = self.to_llvm_return_type(ret_ty).unwrap();

                let param_types: Vec<BasicTypeEnum> = params
                    .iter()
                    .map(|p| self.to_llvm_basic_type(&p.ty).unwrap())
                    .collect();

                let meta: Vec<BasicMetadataTypeEnum> =
                    param_types.iter().map(|t| (*t).into()).collect();

                let fn_type = match ret_type {
                    AnyTypeEnum::VoidType(void_ty) => void_ty.fn_type(&meta, false),
                    AnyTypeEnum::IntType(int_ty) => int_ty.fn_type(&meta, false),
                    AnyTypeEnum::FloatType(float_ty) => float_ty.fn_type(&meta, false),
                    AnyTypeEnum::PointerType(ptr_ty) => ptr_ty.fn_type(&meta, false),
                    AnyTypeEnum::StructType(st_ty) => st_ty.fn_type(&meta, false),
                    _ => return Err(BuilderError::ExtractOutOfRange),
                };

                let function = self.module.add_function(name, fn_type, None);
                self.functions.insert(name.clone(), function);

                let entry = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry);

                for (i, param) in params.iter().enumerate() {
                    let llvm_param = function.get_nth_param(i as u32).unwrap();
                    llvm_param.set_name(&param.name);
                    let alloca = self.builder.build_alloca(param_types[i], &param.name)?;
                    let _ = self.builder.build_store(alloca, llvm_param);
                    self.insert_var(param.name.clone(), alloca, param_types[i], param.ty.clone());
                }

                let prev_fn = self.current_function;
                self.current_function = Some(function);
                self.compile_stmt(body).unwrap();
                self.current_function = prev_fn;
                Ok(())
            }

            HIRDeclaration::Class { .. } => todo!(),
            HIRDeclaration::Enum { .. } => todo!(),
            HIRDeclaration::Include { .. } => Ok(()),
        }
    }

    pub fn compile_stmt(&mut self, stmt: &HIRStmt) -> Result<(), EcoString> {
        match stmt {
            HIRStmt::Expr { expr, .. } => {
                let _ = self.compile_expr(expr)?;
                Ok(())
            }

            HIRStmt::Block { stmts, .. } => {
                self.push_scope();
                for s in stmts {
                    self.compile_stmt(s)?;
                }
                self.pop_scope();
                Ok(())
            }

            HIRStmt::VarDecl { name, ty, init, .. } => {
                let llvm_type = self.to_llvm_basic_type(ty)?;

                let alloca = if let Some(current_fn) = self.current_function {
                    let entry = current_fn.get_first_basic_block().unwrap();

                    let entry_builder = self.context.create_builder();

                    if let Some(first_instr) = entry.get_first_instruction() {
                        entry_builder.position_before(&first_instr);
                    } else {
                        entry_builder.position_at_end(entry);
                    }

                    entry_builder.build_alloca(llvm_type, name)
                } else {
                    self.builder.build_alloca(llvm_type, name)
                }
                .unwrap();

                if let Some(init_expr) = init {
                    let val = self.compile_expr(init_expr)?;
                    let casted = self.cast_to_type(val, ty)?;
                    let _ = self.builder.build_store(alloca, casted);
                }

                self.insert_var(name.clone(), alloca, llvm_type, ty.clone());
                Ok(())
            }
            HIRStmt::Return { expr, .. } => {
                if let Some(e) = expr {
                    let val = self.compile_expr(e)?;
                    let current_fn = self
                        .current_function
                        .ok_or_else(|| "Return outside function")?;

                    // this could've been goofier
                    let fn_ret_ty = {
                        let fn_type = current_fn.get_type();
                        let llvm_ret_type = fn_type
                            .get_return_type()
                            .ok_or_else(|| "Function has no return type")?;
                        self.from_llvm_basic_type(llvm_ret_type)?
                    };

                    let casted_val = self.cast_to_type(val, &fn_ret_ty)?;
                    let _ = self.builder.build_return(Some(&casted_val));
                } else {
                    if self.current_function.is_none() {
                        return Err("Return outside function".into());
                    }
                    let _ = self.builder.build_return(None);
                }
                Ok(())
            }

            HIRStmt::If {
                cond_then_ladder,
                else_branch,
                ..
            } => self.compile_if(cond_then_ladder, else_branch),

            HIRStmt::Switch {
               // expr, cases, nobounds,
                ..
            } => todo!(), // self.compile_switch(expr, cases, *nobounds),

            HIRStmt::While { cond, body, .. } => self.compile_while(cond, body),

            HIRStmt::For {
                init,
                cond,
                step,
                body,
                ..
            } => self.compile_for(init, cond, step, body),

            HIRStmt::Break { .. } => todo!(),
            HIRStmt::Continue { .. } => todo!(),
        }
    }

    fn compile_expr(&mut self, expr: &HIRExpr) -> Result<BasicValueEnum<'ctx>, EcoString> {
        match expr {
            HIRExpr::Ident { name, .. } => {
                if let Some(var) = self.lookup_var(name) {
                    Ok(self
                        .builder
                        .build_load(var.element_type, var.ptr, "loadtmp")
                        .unwrap())
                } else {
                    Err(format!("Unknown variable {}", name).into())
                }
            }
            HIRExpr::Constant { value, .. } => self.compile_constant(value),
            HIRExpr::Binary { op, lhs, rhs, .. } => {
                let l_val = self.compile_expr(lhs)?;
                let r_val = self.compile_expr(rhs)?;
                let expr_ty = expr.ty();
                let l_val = self.cast_to_type(l_val, &expr_ty)?;
                let r_val = self.cast_to_type(r_val, &expr_ty)?;
                let res: BasicValueEnum = match expr.ty() {
                    HIRType::Inbuilt(InbuiltType::F64) => {
                        let l_float = l_val.into_float_value();
                        let r_float = r_val.into_float_value();
                        let res: BasicValueEnum<'ctx> = match op {
                            BinaryOp::Add => self
                                .builder
                                .build_float_add(l_float, r_float, "faddtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Sub => self
                                .builder
                                .build_float_sub(l_float, r_float, "fsubtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Mul => self
                                .builder
                                .build_float_mul(l_float, r_float, "fmultmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Div => self
                                .builder
                                .build_float_div(l_float, r_float, "fdivtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Eq => self
                                .builder
                                .build_float_compare(OEQ, l_float, r_float, "feqtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Ne => self
                                .builder
                                .build_float_compare(ONE, l_float, r_float, "fnetmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Lt => self
                                .builder
                                .build_float_compare(OLT, l_float, r_float, "flttmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Le => self
                                .builder
                                .build_float_compare(OLE, l_float, r_float, "fletmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Gt => self
                                .builder
                                .build_float_compare(OGT, l_float, r_float, "fgttmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Ge => self
                                .builder
                                .build_float_compare(OGE, l_float, r_float, "fgetmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            _ => return Err("Unsupported float binary operator".into()),
                        };
                        res
                    }

                    HIRType::Inbuilt(t)
                        if matches!(
                            t,
                            InbuiltType::I8
                                | InbuiltType::I16
                                | InbuiltType::I32
                                | InbuiltType::I64
                                | InbuiltType::U8
                                | InbuiltType::U16
                                | InbuiltType::U32
                                | InbuiltType::U64
                        ) =>
                    {
                        let l_int = l_val.into_int_value();
                        let r_int = r_val.into_int_value();
                        let signed = matches!(
                            t,
                            InbuiltType::I8
                                | InbuiltType::I16
                                | InbuiltType::I32
                                | InbuiltType::I64
                        );
                        match op {
                            BinaryOp::Add => self
                                .builder
                                .build_int_add(l_int, r_int, "iaddtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Sub => self
                                .builder
                                .build_int_sub(l_int, r_int, "isubtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Mul => self
                                .builder
                                .build_int_mul(l_int, r_int, "imultmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Div => {
                                if signed {
                                    self.builder
                                        .build_int_signed_div(l_int, r_int, "idivtmp")
                                        .map_err(|e| -> EcoString { e.to_string().into() })?
                                        .into()
                                } else {
                                    self.builder
                                        .build_int_unsigned_div(l_int, r_int, "idivtmp")
                                        .map_err(|e| -> EcoString { e.to_string().into() })?
                                        .into()
                                }
                            }
                            BinaryOp::Eq => self
                                .builder
                                .build_int_compare(EQ, l_int, r_int, "ieqtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Ne => self
                                .builder
                                .build_int_compare(NE, l_int, r_int, "inetmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Lt => self
                                .builder
                                .build_int_compare(
                                    if signed { SLT } else { ULT },
                                    l_int,
                                    r_int,
                                    "ilttmp",
                                )
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Le => self
                                .builder
                                .build_int_compare(
                                    if signed { SLE } else { ULE },
                                    l_int,
                                    r_int,
                                    "iletmp",
                                )
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Gt => self
                                .builder
                                .build_int_compare(
                                    if signed { SGT } else { UGT },
                                    l_int,
                                    r_int,
                                    "igttmp",
                                )
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Ge => self
                                .builder
                                .build_int_compare(
                                    if signed { SGE } else { UGE },
                                    l_int,
                                    r_int,
                                    "igetmp",
                                )
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            _ => return Err("Unsupported integer binary operator".into()),
                        }
                    }
                    HIRType::Inbuilt(InbuiltType::Bool) => {
                        let l_bool = l_val.into_int_value();
                        let r_bool = r_val.into_int_value();
                        match op {
                            BinaryOp::And => self
                                .builder
                                .build_and(l_bool, r_bool, "andtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Or => self
                                .builder
                                .build_or(l_bool, r_bool, "ortmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Eq => self
                                .builder
                                .build_int_compare(EQ, l_bool, r_bool, "beqtmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            BinaryOp::Ne => self
                                .builder
                                .build_int_compare(NE, l_bool, r_bool, "bnetmp")
                                .map_err(|e| -> EcoString { e.to_string().into() })?
                                .into(),
                            _ => return Err("Unsupported bool binary operator".into()),
                        }
                    }
                    HIRType::Pointer(_) => return Err("Pointers aren't yet supported!".into()),
                    _ => return Err("Unsupported type for binary operation".into()),
                };
                Ok(res)
            }
            HIRExpr::Unary { op, expr, .. } => {
                let v = self.compile_expr(expr)?;
                match op {
                    UnaryOp::Minus => Ok(self
                        .builder
                        .build_int_neg(v.into_int_value(), "negtmp")
                        .unwrap()
                        .into()),
                    UnaryOp::LogNot => Ok(self
                        .builder
                        .build_not(v.into_int_value(), "nottmp")
                        .unwrap()
                        .into()),
                    _ => Err("Unsupported unary operator".into()),
                }
            }
            HIRExpr::Assign {
                op: _op, /* todo */
                lhs,
                rhs,
                ..
            } => {
                if let HIRExpr::Ident { name, .. } = lhs.as_ref() {
                    let rhs_val = self.compile_expr(rhs)?;
                    if let Some(var) = self.lookup_var(name) {
                        let casted = self.cast_to_type(rhs_val, &var.native_type)?;
                        let _ = self.builder.build_store(var.ptr, casted);
                        Ok(rhs_val)
                    } else {
                        Err(format!("Undefined variable {}", name).into())
                    }
                } else {
                    Err("Unsupported lvalue in assignment".into())
                }
            }
            HIRExpr::Call { func, args, .. } => {
                let func_val = *self
                    .functions
                    .get(func.as_str())
                    .ok_or_else(|| format!("Function {} not found", func))?;

                let fn_type = func_val.get_type();
                let param_types = fn_type.get_param_types();

                let mut compiled_args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(args.len());

                // since varargs are not yet standard
                if func == "Print" {
                    if args.is_empty() {
                        return Err("printf requires at least a format string".into());
                    }

                    let first_val = self.compile_expr(&args[0])?;
                    let first_ty = basic_metadata_to_basic_type(param_types[0]);
                    let target_ty = self.from_llvm_basic_type(first_ty)?;
                    let casted_first = self.cast_to_type(first_val, &target_ty)?;
                    compiled_args.push(casted_first.as_basic_value_enum().into());

                    for arg in &args[1..] {
                        let val = self.compile_expr(arg)?;
                        compiled_args.push(val.as_basic_value_enum().into());
                    }
                } else {
                    if args.len() != param_types.len() {
                        return Err(format!(
                            "Function {} expects {} args, got {}",
                            func,
                            param_types.len(),
                            args.len()
                        )
                        .into());
                    }

                    for (i, arg) in args.iter().enumerate() {
                        let val = self.compile_expr(arg)?;
                        let basic_ty = basic_metadata_to_basic_type(param_types[i]);
                        let target_ty = self.from_llvm_basic_type(basic_ty)?;
                        let casted_val = self.cast_to_type(val, &target_ty)?;
                        compiled_args.push(casted_val.as_basic_value_enum().into());
                    }
                }

                let call_site = self
                    .builder
                    .build_call(func_val, &compiled_args, "calltmp")
                    .unwrap();
                let call_val = call_site.try_as_basic_value().left().unwrap();
                Ok(call_val)
            }
            HIRExpr::ArrElems { .. } => todo!(),
            HIRExpr::Index { .. } => todo!(),
            HIRExpr::Conditional { .. } => todo!(),
            HIRExpr::Cast { .. } => todo!(),
            HIRExpr::Member { .. } => todo!(),
        }
    }

    fn compile_const_expr(&self, expr: &HIRExpr) -> Result<BasicValueEnum<'ctx>, EcoString> {
        use ast::ast::Constant;
        match expr {
            HIRExpr::Constant { value, .. } => match value {
                Constant::Int(i) => Ok(self.context.i32_type().const_int(*i as u64, true).into()),
                Constant::Float(f) => Ok(self.context.f64_type().const_float(*f).into()),
                Constant::Bool(b) => {
                    Ok(self.context.bool_type().const_int(*b as u64, false).into())
                }
                _ => Err("unsupported const initializer".into()),
            },
            _ => Err("non-constant initializer for global".into()),
        }
    }

    fn compile_condition(&mut self, expr: &HIRExpr) -> Result<IntValue<'ctx>, EcoString> {
        let val = self.compile_expr(expr)?;
        match val {
            BasicValueEnum::IntValue(iv) => {
                if iv.get_type().get_bit_width() == 1 {
                    Ok(iv)
                } else {
                    let zero = iv.get_type().const_int(0, false);
                    let cmp = self
                        .builder
                        .build_int_compare(IntPredicate::NE, iv, zero, "ifcond");
                    Ok(cmp.unwrap())
                }
            }
            BasicValueEnum::FloatValue(fv) => {
                let zero = self.context.f64_type().const_float(0.0);
                let cmp = self
                    .builder
                    .build_float_compare(inkwell::FloatPredicate::ONE, fv, zero, "ifcondf")
                    .unwrap();
                Ok(cmp)
            }
            _ => Err("condition must be integer or float (or bool)".into()),
        }
    }

    fn compile_if(
        &mut self,
        cond_then_ladder: &[(HIRExpr, HIRStmt)],
        else_branch: &Option<Box<HIRStmt>>,
    ) -> Result<(), EcoString> {
        let parent_fn = self.current_function.ok_or_else(|| "if outside function")?;
        let merge_bb = self.context.append_basic_block(parent_fn, "ifcont");
        let mut next_bb = self.context.append_basic_block(parent_fn, "if.next");
        if let Some(curr_bb) = self.builder.get_insert_block() {
            if curr_bb.get_terminator().is_none() {
                let _ = self.builder.build_unconditional_branch(next_bb);
            }
        }

        for (i, (cond, stmt)) in cond_then_ladder.iter().enumerate() {
            let this_then = self
                .context
                .append_basic_block(parent_fn, &format!("if_then_{}", i));
            let this_next = self
                .context
                .append_basic_block(parent_fn, &format!("if_next_{}", i));

            self.builder.position_at_end(next_bb);

            let cond_val = self.compile_condition(cond)?;
            let _ = self
                .builder
                .build_conditional_branch(cond_val, this_then, this_next);

            self.builder.position_at_end(this_then);
            self.compile_stmt(stmt)?;
            if self
                .builder
                .get_insert_block()
                .and_then(|b| b.get_terminator())
                .is_none()
            {
                let _ = self.builder.build_unconditional_branch(merge_bb);
            }

            next_bb = this_next;
        }

        self.builder.position_at_end(next_bb);
        if let Some(else_stmt) = else_branch {
            self.compile_stmt(else_stmt)?;
            if self
                .builder
                .get_insert_block()
                .and_then(|b| b.get_terminator())
                .is_none()
            {
                let _ = self.builder.build_unconditional_branch(merge_bb);
            }
        } else {
            let _ = self.builder.build_unconditional_branch(merge_bb);
        }

        self.builder.position_at_end(merge_bb);
        Ok(())
    }

    fn compile_for(
        &mut self,
        init: &Option<Box<HIRStmt>>,
        cond: &Option<HIRExpr>,
        step: &Option<HIRExpr>,
        body: &HIRStmt,
    ) -> Result<(), EcoString> {
        let parent_fn = self
            .current_function
            .ok_or_else(|| "This `for` is outside any function")?;

        self.push_scope();

        if let Some(init_stmt) = init {
            self.compile_stmt(init_stmt)?;
        }

        // basic bloccccs
        let cond_bb = self.context.append_basic_block(parent_fn, "for_cond");
        let body_bb = self.context.append_basic_block(parent_fn, "for_body");
        let step_bb = self.context.append_basic_block(parent_fn, "for_step");
        let after_bb = self.context.append_basic_block(parent_fn, "for_end");

        let _ = self.builder.build_unconditional_branch(cond_bb);

        self.builder.position_at_end(cond_bb);
        let cond_val = if let Some(c) = cond {
            self.compile_condition(c)?
        } else {
            self.context.bool_type().const_int(1, false) // true for infinite loop
        };
        let _ = self
            .builder
            .build_conditional_branch(cond_val, body_bb, after_bb);

        self.builder.position_at_end(body_bb);
        self.compile_stmt(body)?;

        if self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_terminator())
            .is_none()
        {
            let _ = self.builder.build_unconditional_branch(step_bb);
        }

        self.builder.position_at_end(step_bb);

        if let Some(s) = step {
            self.compile_expr(s)?;
        }

        let _ = self.builder.build_unconditional_branch(cond_bb);
        self.builder.position_at_end(after_bb);
        self.pop_scope();

        Ok(())
    }

    #[allow(dead_code)]
    fn compile_switch(
        &mut self,
        expr: &HIRExpr,
        cases: &[HIRCase],
        nobounds: bool,
    ) -> Result<(), EcoString> {
        let cond_val = self.compile_expr(expr)?.into_int_value();
        let parent_fn = self.current_function.unwrap();

        let mut case_blocks = Vec::new();
        let mut default_bb = None;

        for (i, case) in cases.iter().enumerate() {
            let bb = self
                .context
                .append_basic_block(parent_fn, &format!("case{}", i));
            if case.values.is_none() {
                default_bb = Some(bb);
            }
            case_blocks.push((case, bb));
        }

        let merge_bb = self.context.append_basic_block(parent_fn, "switch_merge");

        let mut llvm_cases = Vec::new();
        for (case, bb) in &case_blocks {
            if let Some(values) = &case.values {
                for v in values {
                    let val = self.compile_constant(v)?;
                    llvm_cases.push((val.into_int_value(), *bb));
                }
            }
        }

        let _ = self
            .builder
            .build_switch(cond_val, default_bb.unwrap_or(merge_bb), &llvm_cases);

        for (i, (case, bb)) in case_blocks.iter().enumerate() {
            self.builder.position_at_end(*bb);
            self.compile_stmt(&case.body)?;
            if self
                .builder
                .get_insert_block()
                .unwrap()
                .get_terminator()
                .is_none()
            {
                if nobounds {
                    if let Some((_, next_bb)) = case_blocks.get(i + 1) {
                        let _ = self.builder.build_unconditional_branch(*next_bb);
                    } else {
                        let _ = self.builder.build_unconditional_branch(merge_bb);
                    }
                } else {
                    let _ = self.builder.build_unconditional_branch(merge_bb);
                }
            }
        }

        self.builder.position_at_end(merge_bb);
        Ok(())
    }

    fn compile_while(&mut self, cond: &HIRExpr, body: &HIRStmt) -> Result<(), EcoString> {
        let parent_fn = self
            .current_function
            .ok_or_else(|| "This `while` is outside any function")?;

        self.push_scope();

        let cond_bb = self.context.append_basic_block(parent_fn, "while_cond");
        let body_bb = self.context.append_basic_block(parent_fn, "while_body");
        let after_bb = self.context.append_basic_block(parent_fn, "while_end");

        let _ = self.builder.build_unconditional_branch(cond_bb);

        self.builder.position_at_end(cond_bb);
        let cond_val = self.compile_condition(cond)?;
        let _ = self
            .builder
            .build_conditional_branch(cond_val, body_bb, after_bb);

        self.builder.position_at_end(body_bb);
        self.compile_stmt(body)?;
        if self
            .builder
            .get_insert_block()
            .and_then(|b| b.get_terminator())
            .is_none()
        {
            let _ = self.builder.build_unconditional_branch(cond_bb);
        }

        self.builder.position_at_end(after_bb);
        self.pop_scope();

        Ok(())
    }

    fn compile_constant(&mut self, constant: &Constant) -> Result<BasicValueEnum<'ctx>, EcoString> {
        let val: BasicValueEnum<'ctx> = match constant {
            Constant::Int(v) => self.context.i64_type().const_int(*v as u64, true).into(),
            Constant::UInt(v) => self.context.i64_type().const_int(*v, false).into(),
            Constant::Float(v) => self.context.f64_type().const_float(*v).into(),
            Constant::Char(v) => self.context.i8_type().const_int(*v as u64, false).into(),
            Constant::Bool(v) => self
                .context
                .bool_type()
                .const_int(if *v { 1 } else { 0 }, false)
                .into(),
            Constant::String(v) => {
                let bytes = CString::new(v.as_str()).unwrap();
                let c_str = bytes.as_bytes_with_nul();

                let i8_type = self.context.i8_type();
                let array_type = i8_type.array_type(c_str.len() as u32);

                let global = self.module.add_global(
                    array_type,
                    None,
                    &format!("str.{}", self.strings_counter),
                );
                self.strings_counter += 1;

                let const_array = i8_type.const_array(
                    &c_str
                        .iter()
                        .map(|&b| i8_type.const_int(b as u64, false))
                        .collect::<Vec<_>>(),
                );
                global.set_initializer(&const_array);
                global.set_constant(true);

                global
                    .as_pointer_value()
                    .const_cast(self.context.ptr_type(AddressSpace::default()))
                    .into()
            }
        };
        Ok(val.into())
    }

    // standard function, might change later
    pub fn declare_printf(&mut self) {
        let i32_type = self.context.i32_type();
        let i8_ptr_type = self.context.ptr_type(AddressSpace::default());

        let printf_type =
            i32_type.fn_type(&[BasicMetadataTypeEnum::PointerType(i8_ptr_type)], true);

        // this is kinda crazy, TODO find a slightly better approach MAYBE?
        let libc_printf = self.module.add_function("printf", printf_type, None);

        let printf_wrapper = self.module.add_function("Print", printf_type, None);

        let entry = self.context.append_basic_block(printf_wrapper, "entry");
        let builder = self.context.create_builder();
        builder.position_at_end(entry);

        let args: Vec<_> = (0..printf_wrapper.count_params())
            .map(|i| printf_wrapper.get_nth_param(i).unwrap().into())
            .collect();

        let call_result = builder
            .build_call(libc_printf, &args, "printf_call")
            .unwrap();

        if let Some(return_value) = call_result.try_as_basic_value().left() {
            builder.build_return(Some(&return_value)).unwrap();
        } else {
            let zero = i32_type.const_int(0, false);
            builder.build_return(Some(&zero)).unwrap();
        }

        self.functions.insert("Print".into(), printf_wrapper);
    }
    fn cast_to_type(
        &self,
        value: BasicValueEnum<'ctx>,
        target_type: &HIRType,
    ) -> Result<BasicValueEnum<'ctx>, EcoString> {
        let target_llvm = self.to_llvm_basic_type(target_type)?;

        match (value, target_llvm) {
            (val, target) if val.get_type() == target => Ok(val),

            (BasicValueEnum::IntValue(iv), BasicTypeEnum::IntType(target_int)) => Ok(self
                .builder
                .build_int_cast(iv, target_int, "casttmp")
                .unwrap()
                .into()),

            (BasicValueEnum::FloatValue(fv), BasicTypeEnum::FloatType(target_float)) => Ok(self
                .builder
                .build_float_cast(fv, target_float, "casttmp")
                .unwrap()
                .into()),

            (BasicValueEnum::IntValue(iv), BasicTypeEnum::FloatType(target_float)) => {
                let is_signed = matches!(
                    target_type,
                    HIRType::Inbuilt(
                        InbuiltType::I8 | InbuiltType::I16 | InbuiltType::I32 | InbuiltType::I64
                    )
                );
                let cast = if is_signed {
                    self.builder
                        .build_signed_int_to_float(iv, target_float, "casttmp")
                } else {
                    self.builder
                        .build_unsigned_int_to_float(iv, target_float, "casttmp")
                };
                Ok(cast.unwrap().into())
            }

            (BasicValueEnum::FloatValue(fv), BasicTypeEnum::IntType(target_int)) => {
                let is_signed = matches!(
                    target_type,
                    HIRType::Inbuilt(
                        InbuiltType::I8 | InbuiltType::I16 | InbuiltType::I32 | InbuiltType::I64
                    )
                );
                let cast = if is_signed {
                    self.builder
                        .build_float_to_signed_int(fv, target_int, "casttmp")
                } else {
                    self.builder
                        .build_float_to_unsigned_int(fv, target_int, "casttmp")
                };
                Ok(cast.unwrap().into())
            }

            // fallback
            (val, _) => Ok(val),
        }
    }

    fn from_llvm_basic_type(&self, llvm_ty: BasicTypeEnum<'ctx>) -> Result<HIRType, EcoString> {
        match llvm_ty {
            BasicTypeEnum::IntType(t) => match t.get_bit_width() {
                8 => Ok(HIRType::Inbuilt(InbuiltType::I8)),
                16 => Ok(HIRType::Inbuilt(InbuiltType::I16)),
                32 => Ok(HIRType::Inbuilt(InbuiltType::I32)),
                64 => Ok(HIRType::Inbuilt(InbuiltType::I64)),
                _ => Err("Unsupported int bit width".into()),
            },
            BasicTypeEnum::FloatType(_) => Ok(HIRType::Inbuilt(InbuiltType::F64)),
            BasicTypeEnum::PointerType(_) => Ok(HIRType::Pointer(Box::new(HIRType::Inbuilt(
                InbuiltType::U8,
            )))),
            _ => Err("Unsupported LLVM type".into()),
        }
    }

    fn to_llvm_basic_type(&self, ty: &HIRType) -> Result<BasicTypeEnum<'ctx>, EcoString> {
        match ty {
            HIRType::Inbuilt(i) => match i {
                InbuiltType::U0 => Err("U0/void is not a basic type".into()),
                InbuiltType::U8 | InbuiltType::I8 => Ok(self.context.i8_type().into()),
                InbuiltType::U16 | InbuiltType::I16 => Ok(self.context.i16_type().into()),
                InbuiltType::U32 | InbuiltType::I32 => Ok(self.context.i32_type().into()),
                InbuiltType::U64 | InbuiltType::I64 => Ok(self.context.i64_type().into()),
                InbuiltType::F64 => Ok(self.context.f64_type().into()),
                InbuiltType::Bool => Ok(self.context.bool_type().into()),
            },
            HIRType::Named(name) => self
                .struct_types
                .get(name)
                .map(|t| t.as_basic_type_enum())
                .ok_or_else(|| format!("Unknown named type: {}", name).into()),
            HIRType::Pointer(_inner) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            HIRType::Array { .. } => todo!(),
            HIRType::Function { .. } => Err("Function types are not basic types".into()),
            HIRType::Reference { .. } => todo!(),
            HIRType::Void => todo!(),
        }
    }

    fn to_llvm_return_type(
        &self,
        ty: &HIRType,
    ) -> Result<inkwell::types::AnyTypeEnum<'ctx>, EcoString> {
        match ty {
            HIRType::Inbuilt(InbuiltType::U0) => Ok(self.context.void_type().as_any_type_enum()),
            _ => Ok(self.to_llvm_basic_type(ty)?.as_any_type_enum()),
        }
    }

    fn push_scope(&mut self) {
        self.variables.push(HashMap::new());
    }
    fn pop_scope(&mut self) {
        self.variables.pop();
    }
    fn insert_var(
        &mut self,
        name: EcoString,
        ptr: PointerValue<'ctx>,
        ty: BasicTypeEnum<'ctx>,
        native: HIRType,
    ) {
        self.variables.last_mut().unwrap().insert(
            name,
            Variable {
                ptr,
                element_type: ty,
                native_type: native,
            },
        );
    }
    fn lookup_var(&self, name: &EcoString) -> Option<&Variable<'ctx>> {
        for scope in self.variables.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v);
            }
        }
        None
    }
}

// great
fn basic_metadata_to_basic_type<'ctx>(ty: BasicMetadataTypeEnum<'ctx>) -> BasicTypeEnum<'ctx> {
    match ty {
        BasicMetadataTypeEnum::IntType(int_ty) => int_ty.as_basic_type_enum(),
        BasicMetadataTypeEnum::FloatType(float_ty) => float_ty.as_basic_type_enum(),
        BasicMetadataTypeEnum::PointerType(ptr_ty) => ptr_ty.as_basic_type_enum(),
        BasicMetadataTypeEnum::StructType(struct_ty) => struct_ty.as_basic_type_enum(),
        BasicMetadataTypeEnum::ArrayType(array_ty) => array_ty.as_basic_type_enum(),
        BasicMetadataTypeEnum::VectorType(vec_ty) => vec_ty.as_basic_type_enum(),
        _ => todo!(),
    }
}
