use std::collections::HashMap;

use ecow::EcoString;
use inkwell::{
    AddressSpace, FloatPredicate, IntPredicate,
    builder::{Builder, BuilderError},
    context::Context,
    debug_info,
    execution_engine::ExecutionEngine,
    module::Module as LLVMModule,
    types::{BasicType, BasicTypeEnum, FunctionType, PointerType, StructType},
    values::{BasicValueEnum, FunctionValue, PointerValue},
};

use ast::ast::{BinaryOp, Constant, Declaration, Expr, InbuiltType, Module, NodeId, Stmt, Type};

pub struct Codegen<'ctx> {
    context: &'ctx Context,
    module: LLVMModule<'ctx>,
    builder: Builder<'ctx>,
    engine: ExecutionEngine<'ctx>,

    pub variables: HashMap<NodeId, PointerValue<'ctx>>,
    pub functions: HashMap<EcoString, FunctionValue<'ctx>>,
    pub struct_types: HashMap<EcoString, StructType<'ctx>>,
    pub current_function: Option<FunctionValue<'ctx>>,
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
            variables: HashMap::new(),
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
            Declaration::Var {
                id, name, ty, init, ..
            } => {
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
                self.variables.insert(*id, ptr);
                Ok(())
            }

            Declaration::Func {
                id,
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
                    self.variables.insert(param.id, alloca);
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
                let mut last = self.context.i64_type().const_zero().into();
                for s in stmts {
                    last = self.compile_stmt(s)?;
                }
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

            Stmt::VarDecl {
                id, name, ty, init, ..
            } => {
                if let Some(_f) = self.current_function {
                    let llvm_type = self.to_llvm_basic_type(ty).unwrap();
                    let alloca = self.builder.build_alloca(llvm_type, name)?;
                    if let Some(init_expr) = init {
                        let init_val = self
                            .compile_expr(init_expr)
                            .map_err(|_| BuilderError::UnsetPosition)?;
                        let _ = self.builder.build_store(alloca, init_val);
                    }
                    self.variables.insert(*id, alloca);
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
            Expr::Ident { .. } => todo!(),
            Expr::Constant { value, .. } => self.compile_constant(value),
            Expr::Assign { lhs, rhs, .. } => {
                if let Expr::Ident { id, .. } = lhs.as_ref() {
                    let rhs_val = self.compile_expr(rhs)?;
                    if let Some(var_ptr) = self.variables.get(id) {
                        let _ = self.builder.build_store(*var_ptr, rhs_val);
                        Ok(rhs_val)
                    } else {
                        Err("Undefined variable in assignment".into())
                    }
                } else {
                    Err("Complex lvalue not supported yet".into())
                }
            }
            Expr::Binary { .. } => todo!(),
            Expr::Unary { .. } => todo!(),
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
}
