// ignore this file completely
// if you mention it
// something bad happens
// like a volcanic eruption
// can you think of anything worse?

use ast::{
    ast::{Constant, Declaration, Expr, InbuiltType, Module, Stmt, Type},
    span::DUMMY_SPAN,
};
use codegen::{codegen::Codegen, hir::HIRContext};
use ecow::EcoString;
use inkwell::context::Context;

fn main() {
    let context = Context::create();
    let mut codegen =
        Codegen::new(&context, &EcoString::from("test_module")).expect("Failed to create Codegen");

    // I32 x = 0;
    let var_x = Declaration::Var {
        span: DUMMY_SPAN,
        name: EcoString::from("x"),
        ty: Type::inbuilt(InbuiltType::I32),
        init: Some(Expr::constant(DUMMY_SPAN, Constant::Int(0))),
    };

    // U0 main() { I32 y = 1; }
    let func = Declaration::Func {
        span: DUMMY_SPAN,
        name: EcoString::from("main"),
        ret_ty: Type::inbuilt(InbuiltType::U0),
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
