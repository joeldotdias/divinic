// ignore this file completely
// if you mention it
// something bad happens
// like a volcanic eruption
// can you think of anything worse?

use ast::{
    ast::{BinaryOp, Constant, Declaration, Expr, InbuiltType, Module, Stmt, Type},
    span::DUMMY_SPAN,
};
use codegen::{codegen::Codegen, hir::HIRContext};
use ecow::EcoString;
use inkwell::context::Context;

fn main() {
    let context = Context::create();
    let mut codegen =
        Codegen::new(&context, &EcoString::from("test_module")).expect("Failed to create Codegen");
    let module = Module {
        decls: vec![
            // main function
            Declaration::Func {
                span: DUMMY_SPAN,
                name: EcoString::from("main"),
                ret_ty: Type::inbuilt(InbuiltType::U0),
                params: vec![],
                body: Stmt::block(
                    DUMMY_SPAN,
                    vec![
                        // outer for loop: for i in 1..=5
                        Stmt::for_stmt(
                            DUMMY_SPAN,
                            Some(Stmt::VarDecl {
                                span: DUMMY_SPAN,
                                name: "i".into(),
                                ty: Type::inbuilt(InbuiltType::U32),
                                init: Some(Expr::constant(DUMMY_SPAN, Constant::UInt(1))),
                            }),
                            Some(Expr::binary(
                                DUMMY_SPAN,
                                BinaryOp::Le,
                                Expr::ident(DUMMY_SPAN, "i".into()),
                                Expr::constant(DUMMY_SPAN, Constant::UInt(5)),
                            )),
                            Some(Expr::assign(
                                DUMMY_SPAN,
                                Expr::ident(DUMMY_SPAN, "i".into()),
                                Expr::binary(
                                    DUMMY_SPAN,
                                    BinaryOp::Add,
                                    Expr::ident(DUMMY_SPAN, "i".into()),
                                    Expr::constant(DUMMY_SPAN, Constant::UInt(1)),
                                ),
                            )),
                            Stmt::block(
                                DUMMY_SPAN,
                                vec![
                                    // inner for loop: for j in 0..i
                                    Stmt::for_stmt(
                                        DUMMY_SPAN,
                                        Some(Stmt::VarDecl {
                                            span: DUMMY_SPAN,
                                            name: "j".into(),
                                            ty: Type::inbuilt(InbuiltType::U32),
                                            init: Some(Expr::constant(
                                                DUMMY_SPAN,
                                                Constant::UInt(0),
                                            )),
                                        }),
                                        Some(Expr::binary(
                                            DUMMY_SPAN,
                                            BinaryOp::Lt,
                                            Expr::ident(DUMMY_SPAN, "j".into()),
                                            Expr::ident(DUMMY_SPAN, "i".into()),
                                        )),
                                        Some(Expr::assign(
                                            DUMMY_SPAN,
                                            Expr::ident(DUMMY_SPAN, "j".into()),
                                            Expr::binary(
                                                DUMMY_SPAN,
                                                BinaryOp::Add,
                                                Expr::ident(DUMMY_SPAN, "j".into()),
                                                Expr::constant(DUMMY_SPAN, Constant::UInt(1)),
                                            ),
                                        )),
                                        Stmt::expr(
                                            DUMMY_SPAN,
                                            Expr::call(
                                                DUMMY_SPAN,
                                                "Printf".into(),
                                                vec![Expr::constant(
                                                    DUMMY_SPAN,
                                                    Constant::String("* ".into()),
                                                )],
                                            ),
                                        ),
                                    ),
                                    // print newline after inner loop
                                    Stmt::expr(
                                        DUMMY_SPAN,
                                        Expr::call(
                                            DUMMY_SPAN,
                                            "Printf".into(),
                                            vec![Expr::constant(
                                                DUMMY_SPAN,
                                                Constant::String("\n".into()),
                                            )],
                                        ),
                                    ),
                                ],
                            ),
                        ),
                    ],
                ),
            },
        ],
    };

    let hir_modules = HIRContext::make(vec![module]);
    codegen.compile(&hir_modules).expect("Compilation failed");
    codegen.dump_ir();
    codegen.emit_object_file("main.o");
}
