pub mod codegen;
pub mod emitter;
pub mod hir;
pub mod util;

#[cfg(test)]
mod tests {
    use crate::{codegen::Codegen, hir::HIRContext};
    use ast::{
        ast::{BinaryOp, Constant, Declaration, Expr, InbuiltType, Module, Stmt, Type},
        span::DUMMY_SPAN,
    };
    use ecow::EcoString;
    use inkwell::context::Context;

    #[test]
    fn test_if_statement_in_main() {
        let context = Context::create();
        let mut codegen =
            Codegen::new(&context, &"test_module".to_string()).expect("Failed to create Codegen");

        // let x = 0;
        let var_x = Stmt::VarDecl {
            span: DUMMY_SPAN,
            name: EcoString::from("x"),
            ty: Type::inbuilt(InbuiltType::I32),
            init: Some(Expr::constant(DUMMY_SPAN, Constant::Int(0))),
        };

        // if (x == 0) { x = 42; } else { x = 99; }
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
                            BinaryOp::Eq,
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
                        BinaryOp::Eq,
                        Expr::ident(DUMMY_SPAN, EcoString::from("x")),
                        Expr::constant(DUMMY_SPAN, Constant::Int(99)),
                    ),
                }],
            )),
        );

        // return x;
        let return_stmt = Stmt::Return {
            span: DUMMY_SPAN,
            expr: Some(Expr::ident(DUMMY_SPAN, EcoString::from("x"))),
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

    #[test]
    fn test_return_variable() {
        let context = Context::create();
        let mut codegen =
            Codegen::new(&context, &"test_module".to_string()).expect("Failed to create Codegen");

        // I32 foo() { I32 y = 5; return y; }
        let var_decl = Stmt::VarDecl {
            span: DUMMY_SPAN,
            name: EcoString::from("y"),
            ty: Type::inbuilt(InbuiltType::I32),
            init: Some(Expr::constant(DUMMY_SPAN, Constant::Int(5))),
        };

        let return_stmt = Stmt::Return {
            span: DUMMY_SPAN,
            expr: Some(Expr::ident(DUMMY_SPAN, EcoString::from("y"))),
        };

        let func = Declaration::Func {
            span: DUMMY_SPAN,
            name: EcoString::from("foo"),
            ret_ty: Type::inbuilt(InbuiltType::I32),
            params: vec![],
            body: Stmt::block(DUMMY_SPAN, vec![var_decl, return_stmt]),
        };

        let module = Module { decls: vec![func] };
        let hir_modules = HIRContext::make(vec![module]);

        codegen.compile(&hir_modules).expect("Compilation failed");
        codegen.dump_ir();
    }
}
