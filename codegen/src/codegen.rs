use std::collections::HashMap;

use ecow::EcoString;
use inkwell::{
    AddressSpace, IntPredicate,
    builder::{Builder, BuilderError},
    context::Context,
    execution_engine::ExecutionEngine,
    module::Module as LLVMModule,
    types::{BasicType, BasicTypeEnum, FunctionType, StructType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
};

use ast::ast::{BinaryOp, Constant, Declaration, Expr, InbuiltType, Module, Stmt, Type, UnaryOp};

#[derive(Debug, Clone, Copy)]
struct Variable<'ctx> {
    ptr: PointerValue<'ctx>,
    element_type: BasicTypeEnum<'ctx>,
}

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: LLVMModule<'ctx>,
    builder: Builder<'ctx>,
    #[allow(dead_code)]
    engine: ExecutionEngine<'ctx>,

    variables: Vec<HashMap<EcoString, Variable<'ctx>>>,
    functions: HashMap<EcoString, FunctionValue<'ctx>>,
    struct_types: HashMap<EcoString, StructType<'ctx>>,
    current_function: Option<FunctionValue<'ctx>>,
}

enum CodegenType<'ctx> {
    Basic(BasicTypeEnum<'ctx>),
    Function(FunctionType<'ctx>),
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &EcoString) -> Result<Self, String> {
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

    pub fn compile(&mut self, modules: &[Module]) -> Result<(), String> {
        for m in modules {
            for decl in &m.decls {
                self.compile_decl(decl)
                    .map_err(|e| format!("Codegen error: {:?}", e))?;
            }
        }
        Ok(())
    }

    fn compile_decl(&mut self, decl: &Declaration) -> Result<(), BuilderError> {
        match decl {
            Declaration::Var { name, ty, init, .. } => {
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
                        let val = self.compile_expr(init_expr).unwrap();
                        global.set_initializer(&val);
                    } else {
                        global.set_initializer(&llvm_type.const_zero());
                    }
                    global.as_pointer_value()
                };
                self.insert_var(name.clone(), ptr, llvm_type);
                Ok(())
            }

            Declaration::Func {
                name,
                ret_ty,
                params,
                body,
                ..
            } => {
                let ret_type = self.to_llvm_basic_type(ret_ty).unwrap();
                let param_types: Vec<BasicTypeEnum> = params
                    .iter()
                    .map(|p| self.to_llvm_basic_type(&p.ty).unwrap())
                    .collect();
                let fn_type = ret_type.fn_type(
                    &param_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                    false,
                );

                let function = self.module.add_function(name, fn_type, None);
                self.functions.insert(name.clone(), function);

                let entry = self.context.append_basic_block(function, "entry");
                self.builder.position_at_end(entry);

                for (i, param) in params.iter().enumerate() {
                    let llvm_param = function.get_nth_param(i as u32).unwrap();
                    llvm_param.set_name(&param.name);
                    let alloca = self.builder.build_alloca(param_types[i], &param.name)?;
                    let _ = self.builder.build_store(alloca, llvm_param);
                    self.insert_var(param.name.clone(), alloca, param_types[i]);
                }

                let prev_fn = self.current_function;
                self.current_function = Some(function);
                self.compile_stmt(body)?;
                self.current_function = prev_fn;
                Ok(())
            }

            Declaration::Struct { .. } => todo!(),
            Declaration::Enum { .. } => todo!(),
            Declaration::Include { .. } => todo!(),
        }
    }

    fn compile_stmt(&mut self, stmt: &Stmt) -> Result<BasicValueEnum<'ctx>, BuilderError> {
        match stmt {
            Stmt::Expr { expr, .. } => self
                .compile_expr(expr)
                .map_err(|_| BuilderError::UnsetPosition),

            Stmt::Block { stmts, .. } => {
                self.push_scope();
                let mut last = self.context.i64_type().const_zero().into();
                for s in stmts {
                    last = self.compile_stmt(s)?;
                }
                self.pop_scope();
                Ok(last)
            }

            Stmt::Return { expr, .. } => {
                if let Some(e) = expr {
                    let val = self
                        .compile_expr(e)
                        .map_err(|_| BuilderError::UnsetPosition)?;
                    if let Some(_f) = self.current_function {
                        let _ = self.builder.build_return(Some(&val));
                    }
                } else {
                    let _ = self.builder.build_return(None);
                }
                Ok(self.context.i64_type().const_zero().into())
            }

            Stmt::VarDecl { name, ty, init, .. } => {
                if self.current_function.is_some() {
                    let llvm_type = self.to_llvm_basic_type(ty).unwrap();
                    let alloca = self.builder.build_alloca(llvm_type, name)?;
                    if let Some(init_expr) = init {
                        let init_val = self
                            .compile_expr(init_expr)
                            .map_err(|_| BuilderError::UnsetPosition)?;
                        let _ = self.builder.build_store(alloca, init_val);
                    }
                    self.insert_var(name.clone(), alloca, llvm_type);
                    Ok(alloca.into())
                } else {
                    Err(BuilderError::UnsetPosition)
                }
            }

            Stmt::If { .. } => todo!(),
            Stmt::Switch { .. } => todo!(),
            Stmt::While { .. } => todo!(),
            Stmt::For { .. } => todo!(),
            Stmt::Break { .. } => todo!(),
            Stmt::Continue { .. } => todo!(),
        }
    }

    fn compile_expr(&self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, EcoString> {
        match expr {
            Expr::Ident { name, .. } => {
                if let Some(var) = self.lookup_var(name) {
                    Ok(self
                        .builder
                        .build_load(var.element_type, var.ptr, "loadtmp")
                        .unwrap())
                } else {
                    Err(format!("Unknown variable {}", name).into())
                }
            }

            Expr::Constant { value, .. } => self.compile_constant(value),
            Expr::Assign { lhs, rhs, .. } => {
                if let Expr::Ident { name, .. } = lhs.as_ref() {
                    let rhs_val = self.compile_expr(rhs).unwrap();
                    if let Some(var) = self.lookup_var(name) {
                        let _ = self.builder.build_store(var.ptr, rhs_val);
                        Ok(rhs_val)
                    } else {
                        Err("Undefined variable in assignment".into())
                    }
                } else {
                    Err("Complex lvalue not supported yet".into())
                }
            }
            Expr::Binary { op, lhs, rhs, .. } => {
                let l = self.compile_expr(lhs)?;
                let r = self.compile_expr(rhs)?;
                match op {
                    BinaryOp::Add => Ok(self
                        .builder
                        .build_int_add(l.into_int_value(), r.into_int_value(), "addtmp")
                        .unwrap()
                        .into()),
                    BinaryOp::Sub => Ok(self
                        .builder
                        .build_int_sub(l.into_int_value(), r.into_int_value(), "subtmp")
                        .unwrap()
                        .into()),
                    BinaryOp::Mul => Ok(self
                        .builder
                        .build_int_mul(l.into_int_value(), r.into_int_value(), "multmp")
                        .unwrap()
                        .into()),
                    BinaryOp::Div => Ok(self
                        .builder
                        .build_int_signed_div(l.into_int_value(), r.into_int_value(), "divtmp")
                        .unwrap()
                        .into()),
                    BinaryOp::Eq => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::EQ,
                            l.into_int_value(),
                            r.into_int_value(),
                            "eqtmp",
                        )
                        .unwrap()
                        .into()),
                    BinaryOp::Lt => Ok(self
                        .builder
                        .build_int_compare(
                            IntPredicate::SLT,
                            l.into_int_value(),
                            r.into_int_value(),
                            "lttmp",
                        )
                        .unwrap()
                        .into()),
                    _ => Err("Unsupported binary operator".into()),
                }
            }

            Expr::Unary { op, expr, .. } => {
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

            Expr::Call { .. } => todo!(),
            Expr::Member { .. } => todo!(),
            Expr::Index { .. } => todo!(),
            Expr::Conditional { .. } => todo!(),
            Expr::Cast { .. } => todo!(),
            Expr::Paren { .. } => todo!(),
        }
    }

    fn compile_constant(&self, constant: &Constant) -> Result<BasicValueEnum<'ctx>, EcoString> {
        let val = match constant {
            Constant::Int(value) => self
                .context
                .i64_type()
                .const_int(*value as u64, true)
                .into(),
            Constant::UInt(value) => self.context.i64_type().const_int(*value, false).into(),
            Constant::Float(value) => self.context.f64_type().const_float(*value).into(),
            Constant::Char(value) => self
                .context
                .i8_type()
                .const_int(*value as u64, false)
                .into(),
            Constant::Bool(value) => self
                .context
                .bool_type()
                .const_int(if *value { 1 } else { 0 }, false)
                .into(),
            Constant::String(_) => return Err("String constants not supported yet".into()),
        };
        Ok(val)
    }

    fn to_llvm_type(&self, ty: &Type) -> Result<CodegenType<'ctx>, String> {
        match ty {
            Type::Function { .. } => Ok(CodegenType::Function(self.to_llvm_function_type(ty)?)),
            _ => Ok(CodegenType::Basic(self.to_llvm_basic_type(ty)?)),
        }
    }

    fn to_llvm_basic_type(&self, ty: &Type) -> Result<BasicTypeEnum<'ctx>, String> {
        match ty {
            Type::Inbuilt(i) => match i {
                InbuiltType::U0 => Err("U0/void is not a basic type".to_string()),
                InbuiltType::U8 | InbuiltType::I8 => Ok(self.context.i8_type().into()),
                InbuiltType::U16 | InbuiltType::I16 => Ok(self.context.i16_type().into()),
                InbuiltType::U32 | InbuiltType::I32 => Ok(self.context.i32_type().into()),
                InbuiltType::U64 | InbuiltType::I64 => Ok(self.context.i64_type().into()),
                InbuiltType::F64 => Ok(self.context.f64_type().into()),
                InbuiltType::Bool => Ok(self.context.bool_type().into()),
            },
            Type::Named(name) => {
                if let Some(struct_type) = self.struct_types.get(name) {
                    Ok(struct_type.as_basic_type_enum())
                } else {
                    Err(format!("Unknown named type: {}", name))
                }
            }
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
                        _ => return Err("Array size must be constant integer".to_string()),
                    }
                } else {
                    return Err("Unsized arrays not supported".to_string());
                };
                Ok(inner_ty.array_type(size).into())
            }
            Type::Function { .. } => Err("Function types are not basic types".to_string()),
        }
    }

    fn to_llvm_function_type(&self, ty: &Type) -> Result<FunctionType<'ctx>, String> {
        if let Type::Function {
            params,
            ret,
            varargs,
        } = ty
        {
            let param_types: Result<Vec<_>, _> =
                params.iter().map(|p| self.to_llvm_basic_type(p)).collect();
            let param_types = param_types?;

            let fn_type = match self.to_llvm_type(ret)? {
                CodegenType::Basic(ret_type) => ret_type.fn_type(
                    &param_types.iter().map(|t| (*t).into()).collect::<Vec<_>>(),
                    *varargs,
                ),
                CodegenType::Function(_) => {
                    return Err("Functions cannot return function types directly".to_string());
                }
            };

            Ok(fn_type)
        } else {
            Err("Not a function type".to_string())
        }
    }

    /** helpers **/

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
        element_type: BasicTypeEnum<'ctx>,
    ) {
        if let Some(scope) = self.variables.last_mut() {
            scope.insert(name, Variable { ptr, element_type });
        }
    }

    fn lookup_var(&self, name: &EcoString) -> Option<Variable<'ctx>> {
        for scope in self.variables.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(*var);
            }
        }
        None
    }
}
