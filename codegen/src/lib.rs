pub mod codegen;

#[cfg(test)]
mod tests {
    use crate::codegen::Codegen;
    use ast::{
        ast::{Constant, Declaration, Expr, InbuiltType, Module, NodeId, Stmt, Type},
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
}
