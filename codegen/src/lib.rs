pub mod codegen;

#[cfg(test)]
mod tests {
    use crate::codegen::Codegen;
    use ast::{
        ast::{BinaryOp, Constant, Declaration, Expr, InbuiltType, Module, NodeId, Stmt, Type},
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
            id: 1 as NodeId,
            span: DUMMY_SPAN,
            name: EcoString::from("x"),
            ty: Type::Inbuilt(InbuiltType::I32),
            init: Some(Expr::Constant {
                id: 10 as NodeId,
                span: DUMMY_SPAN,
                value: Constant::Int(0),
            }),
        };

        // 16 stuff() { I32 y = 1; }
        let func = Declaration::Func {
            id: 2 as NodeId,
            span: DUMMY_SPAN,
            name: EcoString::from("stuff"),
            ret_ty: Type::Inbuilt(InbuiltType::I16),
            params: vec![],
            body: Stmt::Block {
                id: 20 as NodeId,
                span: DUMMY_SPAN,
                stmts: vec![Stmt::VarDecl {
                    id: 3 as NodeId,
                    span: DUMMY_SPAN,
                    name: EcoString::from("y"),
                    ty: Type::Inbuilt(InbuiltType::I32),
                    init: Some(Expr::Constant {
                        id: 21 as NodeId,
                        span: DUMMY_SPAN,
                        value: Constant::Int(1),
                    }),
                }],
            },
        };

        let module = Module {
            decls: vec![var_x, func],
        };

        codegen.compile(&[module]).expect("Compilation failed");

        codegen.dump_ir();
    }

    #[test]
    fn test_i8_add2_function() {
        let context = Context::create();
        let mut codegen = crate::codegen::Codegen::new(&context, &EcoString::from("test_module"))
            .expect("Failed to create Codegen");

        // Function: I8 add2(I8 x) { I8 y = x + 2; }
        let func = Declaration::Func {
            id: 1,
            span: DUMMY_SPAN,
            name: EcoString::from("add2"),
            ret_ty: Type::Inbuilt(InbuiltType::I8),
            params: vec![ast::ast::Param {
                id: 10,
                span: DUMMY_SPAN,
                name: EcoString::from("x"),
                ty: Type::Inbuilt(InbuiltType::I8),
            }],
            body: Stmt::Block {
                id: 2,
                span: DUMMY_SPAN,
                stmts: vec![Stmt::VarDecl {
                    id: 3,
                    span: DUMMY_SPAN,
                    name: EcoString::from("y"),
                    ty: Type::Inbuilt(InbuiltType::I8),
                    init: Some(Expr::Binary {
                        id: 4,
                        span: DUMMY_SPAN,
                        op: BinaryOp::Add,
                        lhs: Box::new(Expr::Ident {
                            id: 5,
                            span: DUMMY_SPAN,
                            name: EcoString::from("x"),
                        }),
                        rhs: Box::new(Expr::Constant {
                            id: 6,
                            span: DUMMY_SPAN,
                            value: Constant::Int(2),
                        }),
                    }),
                }],
            },
        };

        let module = Module { decls: vec![func] };

        codegen.compile(&[module]).expect("Compilation failed");
        codegen.dump_ir();
    }
}
