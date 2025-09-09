pub mod codegen;
pub mod hir;

#[cfg(test)]
mod tests {
    use crate::codegen::Codegen;
    use crate::hir::{ast_to_hir, infer_types};
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

        // I16 stuff() { I32 y = 1; }
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

        let mut hir_modules = ast_to_hir(vec![module]);
        infer_types(&mut hir_modules);
        codegen.compile(&hir_modules).expect("Compilation failed");

        codegen.dump_ir();
    }

    #[test]
    fn test_i8_add2_function() {
        let context = Context::create();
        let mut codegen = crate::codegen::Codegen::new(&context, &EcoString::from("test_module"))
            .expect("Failed to create Codegen");

        // I8 add2(I8 x) { I8 y = x + 2; }
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

        let mut hir_modules = ast_to_hir(vec![module]);
        infer_types(&mut hir_modules);
        codegen.compile(&hir_modules).expect("Compilation failed");
        codegen.dump_ir();
    }

    #[test]
    fn test_float_add() {
        let context = Context::create();
        let mut codegen = crate::codegen::Codegen::new(&context, &EcoString::from("test_module"))
            .expect("Failed to create Codegen");

        // F64 add_constants() { F64 y = 1.5 + 2.0; return y; }
        let func = Declaration::Func {
            id: 1,
            span: DUMMY_SPAN,
            name: EcoString::from("add_constants"),
            ret_ty: Type::Inbuilt(InbuiltType::F64),
            params: vec![],
            body: Stmt::Block {
                id: 2,
                span: DUMMY_SPAN,
                stmts: vec![
                    Stmt::VarDecl {
                        id: 3,
                        span: DUMMY_SPAN,
                        name: EcoString::from("y"),
                        ty: Type::Inbuilt(InbuiltType::F64),
                        init: Some(Expr::Binary {
                            id: 4,
                            span: DUMMY_SPAN,
                            op: BinaryOp::Add,
                            lhs: Box::new(Expr::Constant {
                                id: 5,
                                span: DUMMY_SPAN,
                                value: Constant::Float(1.5),
                            }),
                            rhs: Box::new(Expr::Constant {
                                id: 6,
                                span: DUMMY_SPAN,
                                value: Constant::Float(2.0),
                            }),
                        }),
                    },
                    Stmt::Return {
                        id: 7,
                        span: DUMMY_SPAN,
                        expr: Some(Expr::Ident {
                            id: 8,
                            span: DUMMY_SPAN,
                            name: EcoString::from("y"),
                        }),
                    },
                ],
            },
        };

        let module = Module { decls: vec![func] };
        let mut hir_modules = ast_to_hir(vec![module]);
        infer_types(&mut hir_modules);
        codegen.compile(&hir_modules).expect("Compilation failed");
        codegen.dump_ir();
    }
    #[test]
    fn test_if_statement_in_main() {
        let context = Context::create();
        let mut codegen = crate::codegen::Codegen::new(&context, &EcoString::from("test_module"))
            .expect("Failed to create Codegen");

        // I32 main() {
        //   I32 x = 0;
        //   if (x == 0) {
        //       x = 42;
        //   } else {
        //       x = 99;
        //   }
        //   return x;
        // }

        let func = Declaration::Func {
            id: 1,
            span: DUMMY_SPAN,
            name: EcoString::from("main"),
            ret_ty: Type::Inbuilt(InbuiltType::I32),
            params: vec![],
            body: Stmt::Block {
                id: 2,
                span: DUMMY_SPAN,
                stmts: vec![
                    // I32 x = 0;
                    Stmt::VarDecl {
                        id: 3,
                        span: DUMMY_SPAN,
                        name: EcoString::from("x"),
                        ty: Type::Inbuilt(InbuiltType::I32),
                        init: Some(Expr::Constant {
                            id: 4,
                            span: DUMMY_SPAN,
                            value: Constant::Int(0),
                        }),
                    },
                    // if (x == 0) { x = 42; } else { x = 99; }
                    Stmt::If {
                        id: 5,
                        span: DUMMY_SPAN,
                        cond: Expr::Binary {
                            id: 6,
                            span: DUMMY_SPAN,
                            op: BinaryOp::Eq,
                            lhs: Box::new(Expr::Ident {
                                id: 7,
                                span: DUMMY_SPAN,
                                name: EcoString::from("x"),
                            }),
                            rhs: Box::new(Expr::Constant {
                                id: 8,
                                span: DUMMY_SPAN,
                                value: Constant::Int(0),
                            }),
                        },
                        then_branch: Box::new(Stmt::Block {
                            id: 9,
                            span: DUMMY_SPAN,
                            stmts: vec![Stmt::Expr {
                                id: 10,
                                span: DUMMY_SPAN,
                                expr: Expr::Assign {
                                    id: 11,
                                    span: DUMMY_SPAN,
                                    lhs: Box::new(Expr::Ident {
                                        id: 12,
                                        span: DUMMY_SPAN,
                                        name: EcoString::from("x"),
                                    }),
                                    rhs: Box::new(Expr::Constant {
                                        id: 13,
                                        span: DUMMY_SPAN,
                                        value: Constant::Int(42),
                                    }),
                                },
                            }],
                        }),
                        ladder: vec![],
                        else_branch: Some(Box::new(Stmt::Block {
                            id: 14,
                            span: DUMMY_SPAN,
                            stmts: vec![Stmt::Expr {
                                id: 15,
                                span: DUMMY_SPAN,
                                expr: Expr::Assign {
                                    id: 16,
                                    span: DUMMY_SPAN,
                                    lhs: Box::new(Expr::Ident {
                                        id: 17,
                                        span: DUMMY_SPAN,
                                        name: EcoString::from("x"),
                                    }),
                                    rhs: Box::new(Expr::Constant {
                                        id: 18,
                                        span: DUMMY_SPAN,
                                        value: Constant::Int(99),
                                    }),
                                },
                            }],
                        })),
                    },
                    // return x;
                    Stmt::Return {
                        id: 19,
                        span: DUMMY_SPAN,
                        expr: Some(Expr::Ident {
                            id: 20,
                            span: DUMMY_SPAN,
                            name: EcoString::from("x"),
                        }),
                    },
                ],
            },
        };

        let module = Module { decls: vec![func] };
        let mut hir_modules = ast_to_hir(vec![module]);
        infer_types(&mut hir_modules);
        codegen.compile(&hir_modules).expect("Compilation failed");
        codegen.dump_ir();
    }
}
