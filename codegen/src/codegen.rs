use ecow::EcoString;
use inkwell::{
    AddressSpace, IntPredicate,
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module as InkModule,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, StructType},
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
};
use std::collections::HashMap;

use crate::hir::{HIRDeclaration, HIRExpr, HIRExprGives, HIRModule, HIRStmt};
use ast::ast::{BinaryOp, Constant, Expr, InbuiltType, Type, UnaryOp};

#[derive(Debug, Clone)]
struct Variable<'ctx> {
    ptr: PointerValue<'ctx>,
    element_type: BasicTypeEnum<'ctx>,
    native_type: Type,
}

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: InkModule<'ctx>,
    builder: Builder<'ctx>,
    #[allow(dead_code)]
    engine: ExecutionEngine<'ctx>,

    variables: Vec<HashMap<EcoString, Variable<'ctx>>>,
    functions: HashMap<EcoString, FunctionValue<'ctx>>,
    struct_types: HashMap<EcoString, StructType<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &EcoString) -> Result<Self, EcoString> {
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

            HIRDeclaration::Struct { .. } => todo!(),
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
                    let entry = current_fn
                        .get_first_basic_block()
                        .unwrap_or_else(|| self.context.append_basic_block(current_fn, "entry"));
                    let entry_builder = self.context.create_builder();
                    entry_builder.position_at_end(entry);
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

            HIRStmt::Switch { .. } => todo!(),
            HIRStmt::While { .. } => todo!(),

            HIRStmt::For { .. } => todo!(),

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
                let l_val = self.cast_to_type(l_val, &expr.ty())?;
                let r_val = self.cast_to_type(r_val, &expr.ty())?;

                match expr.ty() {
                    Type::Inbuilt(InbuiltType::F64) => {
                        let res = match op {
                            BinaryOp::Add => self
                                .builder
                                .build_float_add(
                                    l_val.into_float_value(),
                                    r_val.into_float_value(),
                                    "addtmp",
                                )
                                .map(|v| v.into()),
                            BinaryOp::Sub => self
                                .builder
                                .build_float_sub(
                                    l_val.into_float_value(),
                                    r_val.into_float_value(),
                                    "subtmp",
                                )
                                .map(|v| v.into()),
                            BinaryOp::Mul => self
                                .builder
                                .build_float_mul(
                                    l_val.into_float_value(),
                                    r_val.into_float_value(),
                                    "multmp",
                                )
                                .map(|v| v.into()),
                            BinaryOp::Div => self
                                .builder
                                .build_float_div(
                                    l_val.into_float_value(),
                                    r_val.into_float_value(),
                                    "divtmp",
                                )
                                .map(|v| v.into()),
                            BinaryOp::Eq => self
                                .builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OEQ,
                                    l_val.into_float_value(),
                                    r_val.into_float_value(),
                                    "eqtmp",
                                )
                                .map(|v| v.into()),
                            BinaryOp::Lt => self
                                .builder
                                .build_float_compare(
                                    inkwell::FloatPredicate::OLT,
                                    l_val.into_float_value(),
                                    r_val.into_float_value(),
                                    "lttmp",
                                )
                                .map(|v| v.into()),
                            _ => return Err("Unsupported float binary operator".into()),
                        }
                        .map_err(|e| format!("Codegen error: {:?}", e))?;

                        Ok(res)
                    }
                    Type::Inbuilt(InbuiltType::I64) | Type::Inbuilt(InbuiltType::I32) => {
                        let res = match op {
                            BinaryOp::Add => self.builder.build_int_add(
                                l_val.into_int_value(),
                                r_val.into_int_value(),
                                "addtmp",
                            ),
                            BinaryOp::Sub => self.builder.build_int_sub(
                                l_val.into_int_value(),
                                r_val.into_int_value(),
                                "subtmp",
                            ),
                            BinaryOp::Mul => self.builder.build_int_mul(
                                l_val.into_int_value(),
                                r_val.into_int_value(),
                                "multmp",
                            ),
                            BinaryOp::Div => self.builder.build_int_signed_div(
                                l_val.into_int_value(),
                                r_val.into_int_value(),
                                "divtmp",
                            ),
                            BinaryOp::Eq => self.builder.build_int_compare(
                                IntPredicate::EQ,
                                l_val.into_int_value(),
                                r_val.into_int_value(),
                                "eqtmp",
                            ),
                            BinaryOp::Lt => self.builder.build_int_compare(
                                IntPredicate::SLT,
                                l_val.into_int_value(),
                                r_val.into_int_value(),
                                "lttmp",
                            ),
                            _ => return Err("Unsupported integer binary operator".into()),
                        }
                        .map_err(|e| format!("Codegen error: {:?}", e))?;
                        Ok(res.into())
                    }

                    _ => Err("Unsupported type for binary op".into()),
                }
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
            HIRExpr::Assign { lhs, rhs, .. } => {
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
            _ => todo!(),
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

    fn compile_constant(&self, constant: &Constant) -> Result<BasicValueEnum<'ctx>, EcoString> {
        let val = match constant {
            Constant::Int(v) => self.context.i64_type().const_int(*v as u64, true).into(),
            Constant::UInt(v) => self.context.i64_type().const_int(*v, false).into(),
            Constant::Float(v) => self.context.f64_type().const_float(*v).into(),
            Constant::Char(v) => self.context.i8_type().const_int(*v as u64, false).into(),
            Constant::Bool(v) => self
                .context
                .bool_type()
                .const_int(if *v { 1 } else { 0 }, false)
                .into(),
            Constant::String(_) => return Err("String constants not supported yet".into()),
        };
        Ok(val)
    }

    fn cast_to_type(
        &self,
        value: BasicValueEnum<'ctx>,
        target_type: &Type,
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
                    Type::Inbuilt(
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
                    Type::Inbuilt(
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

    fn from_llvm_basic_type(&self, llvm_ty: BasicTypeEnum<'ctx>) -> Result<Type, EcoString> {
        match llvm_ty {
            BasicTypeEnum::IntType(t) => match t.get_bit_width() {
                8 => Ok(Type::Inbuilt(InbuiltType::I8)),
                16 => Ok(Type::Inbuilt(InbuiltType::I16)),
                32 => Ok(Type::Inbuilt(InbuiltType::I32)),
                64 => Ok(Type::Inbuilt(InbuiltType::I64)),
                _ => Err("Unsupported int bit width".into()),
            },
            BasicTypeEnum::FloatType(_) => Ok(Type::Inbuilt(InbuiltType::F64)),
            BasicTypeEnum::PointerType(_) => {
                Ok(Type::Pointer(Box::new(Type::Inbuilt(InbuiltType::U8))))
            }
            _ => Err("Unsupported LLVM type".into()),
        }
    }

    fn to_llvm_basic_type(&self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, EcoString> {
        match ty {
            Type::Inbuilt(i) => match i {
                InbuiltType::U0 => Err("U0/void is not a basic type".into()),
                InbuiltType::U8 | InbuiltType::I8 => Ok(self.context.i8_type().into()),
                InbuiltType::U16 | InbuiltType::I16 => Ok(self.context.i16_type().into()),
                InbuiltType::U32 | InbuiltType::I32 => Ok(self.context.i32_type().into()),
                InbuiltType::U64 | InbuiltType::I64 => Ok(self.context.i64_type().into()),
                InbuiltType::F64 => Ok(self.context.f64_type().into()),
                InbuiltType::Bool => Ok(self.context.bool_type().into()),
            },
            Type::Named(name) => self
                .struct_types
                .get(name)
                .map(|t| t.as_basic_type_enum())
                .ok_or_else(|| format!("Unknown named type: {}", name).into()),
            Type::Pointer(_inner) => Ok(self.context.ptr_type(AddressSpace::default()).into()),
            Type::Array(inner, maybe_size_expr) => {
                let inner_ty = self.to_llvm_basic_type(inner)?;
                let size = if let Some(size_expr) = maybe_size_expr {
                    match size_expr {
                        Expr::Constant {
                            value: Constant::Int(n),
                            ..
                        } => *n as u32,
                        Expr::Constant {
                            value: Constant::UInt(u),
                            ..
                        } => *u as u32,
                        _ => return Err("Array size must be a constant integer".into()),
                    }
                } else {
                    return Err("Unsized arrays not supported".into());
                };
                Ok(inner_ty.array_type(size).into())
            }
            Type::Function { .. } => Err("Function types are not basic types".into()),
        }
    }

    fn to_llvm_return_type(
        &self,
        ty: &Type,
    ) -> Result<inkwell::types::AnyTypeEnum<'ctx>, EcoString> {
        match ty {
            Type::Inbuilt(InbuiltType::U0) => Ok(self.context.void_type().as_any_type_enum()),
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
        native: Type,
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
