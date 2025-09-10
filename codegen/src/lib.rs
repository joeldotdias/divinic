pub mod codegen;
pub mod hir;

#[cfg(test)]
mod tests {
    use crate::{codegen::Codegen, hir::HIRContext};
    use ast::{
        ast::{BinaryOp, Constant, Declaration, Expr, InbuiltType, Module, Param, Stmt, Type},
        span::DUMMY_SPAN,
    };
    use ecow::EcoString;
    use inkwell::context::Context;

    #[test]
    fn test_globals_and_function_with_local() {
        let context = Context::create();
        let mut codegen = Codegen::new(&context, &EcoString::from("test_module"))
            .expect("Failed to create Codegen");

        // I32 x = 0;
        let var_x = Declaration::Var {
            span: DUMMY_SPAN,
            name: EcoString::from("x"),
            ty: Type::inbuilt(InbuiltType::I32),
            init: Some(Expr::constant(DUMMY_SPAN, Constant::Int(0))),
        };

        // I16 stuff() { I32 y = 1; }
        let func = Declaration::Func {
            span: DUMMY_SPAN,
            name: EcoString::from("stuff"),
            ret_ty: Type::inbuilt(InbuiltType::I16),
            params: vec![],
            body: Stmt::block(
                DUMMY_SPAN,
                vec![Stmt::VarDecl {
                    span: DUMMY_SPAN,
                    name: EcoString::from("y"),
                    ty: Type::inbuilt(InbuiltType::I32),
                    init: Some(Expr::constant(DUMMY_SPAN, Constant::Int(1))),
                }],
            ),
        };

        let module = Module {
            decls: vec![var_x, func],
        };

        let hir_modules = HIRContext::make(vec![module]);
        codegen.compile(&hir_modules).expect("Compilation failed");
        codegen.dump_ir();
    }

    #[test]
    fn test_i8_add2_function() {
        let context = Context::create();
        let mut codegen = Codegen::new(&context, &EcoString::from("test_module"))
            .expect("Failed to create Codegen");

        let param_x = Param {
            span: DUMMY_SPAN,
            name: EcoString::from("x"),
            ty: Type::inbuilt(InbuiltType::I8),
        };

        let func = Declaration::Func {
            span: DUMMY_SPAN,
            name: EcoString::from("add2"),
            ret_ty: Type::inbuilt(InbuiltType::I8),
            params: vec![param_x],
            body: Stmt::block(
                DUMMY_SPAN,
                vec![Stmt::VarDecl {
                    span: DUMMY_SPAN,
                    name: EcoString::from("y"),
                    ty: Type::inbuilt(InbuiltType::I8),
                    init: Some(Expr::binary(
                        DUMMY_SPAN,
                        BinaryOp::Add,
                        Expr::ident(DUMMY_SPAN, EcoString::from("x")),
                        Expr::constant(DUMMY_SPAN, Constant::Int(2)),
                    )),
                }],
            ),
        };

        let module = Module { decls: vec![func] };

        let hir_modules = HIRContext::make(vec![module]);
        codegen.compile(&hir_modules).expect("Compilation failed");
        codegen.dump_ir();
    }

    #[test]
    fn test_float_add() {
        let context = Context::create();
        let mut codegen = Codegen::new(&context, &EcoString::from("test_module"))
            .expect("Failed to create Codegen");

        let func = Declaration::Func {
            span: DUMMY_SPAN,
            name: EcoString::from("add_constants"),
            ret_ty: Type::inbuilt(InbuiltType::F64),
            params: vec![],
            body: Stmt::block(
                DUMMY_SPAN,
                vec![
                    Stmt::VarDecl {
                        span: DUMMY_SPAN,
                        name: EcoString::from("y"),
                        ty: Type::inbuilt(InbuiltType::F64),
                        init: Some(Expr::binary(
                            DUMMY_SPAN,
                            BinaryOp::Add,
                            Expr::constant(DUMMY_SPAN, Constant::Float(1.5)),
                            Expr::constant(DUMMY_SPAN, Constant::Float(2.0)),
                        )),
                    },
                    Stmt::Expr {
                        span: DUMMY_SPAN,
                        expr: Expr::ident(DUMMY_SPAN, EcoString::from("y")),
                    },
                ],
            ),
        };

        let module = Module { decls: vec![func] };

        let hir_modules = HIRContext::make(vec![module]);
        codegen.compile(&hir_modules).expect("Compilation failed");
        codegen.dump_ir();
    }

    #[test]
    fn test_if_statement_in_main() {
        let context = Context::create();
        let mut codegen = Codegen::new(&context, &EcoString::from("test_module"))
            .expect("Failed to create Codegen");

        let var_x = Stmt::VarDecl {
            span: DUMMY_SPAN,
            name: EcoString::from("x"),
            ty: Type::inbuilt(InbuiltType::I32),
            init: Some(Expr::constant(DUMMY_SPAN, Constant::Int(0))),
        };

        let if_stmt = Stmt::if_stmt(
            DUMMY_SPAN,
            vec![(
                Expr::binary(
                    DUMMY_SPAN,
                    BinaryOp::Eq,
                    Expr::ident(DUMMY_SPAN, EcoString::from("x")),
                    Expr::constant(DUMMY_SPAN, Constant::Int(0)),
                ),
                Stmt::block(
                    DUMMY_SPAN,
                    vec![Stmt::Expr {
                        span: DUMMY_SPAN,
                        expr: Expr::assign(
                            DUMMY_SPAN,
                            Expr::ident(DUMMY_SPAN, EcoString::from("x")),
                            Expr::constant(DUMMY_SPAN, Constant::Int(42)),
                        ),
                    }],
                ),
            )],
            Some(Stmt::block(
                DUMMY_SPAN,
                vec![Stmt::Expr {
                    span: DUMMY_SPAN,
                    expr: Expr::assign(
                        DUMMY_SPAN,
                        Expr::ident(DUMMY_SPAN, EcoString::from("x")),
                        Expr::constant(DUMMY_SPAN, Constant::Int(99)),
                    ),
                }],
            )),
        );

        let return_stmt = Stmt::Expr {
            span: DUMMY_SPAN,
            expr: Expr::ident(DUMMY_SPAN, EcoString::from("x")),
        };

        let func = Declaration::Func {
            span: DUMMY_SPAN,
            name: EcoString::from("main"),
            ret_ty: Type::inbuilt(InbuiltType::I32),
            params: vec![],
            body: Stmt::block(DUMMY_SPAN, vec![var_x, if_stmt, return_stmt]),
        };

        let module = Module { decls: vec![func] };

        let hir_modules = HIRContext::make(vec![module]);
        codegen.compile(&hir_modules).expect("Compilation failed");
        codegen.dump_ir();
    }
}
