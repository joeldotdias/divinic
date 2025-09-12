use std::env;

use codegen::{
    codegen::{Codegen, CodegenContext},
    hir::HIRContext,
};
use parse::session::ParseSess;

fn main() {
    let files = if env::args().len() > 1 {
        env::args().skip(1).collect::<Vec<String>>()
    } else {
        vec![
            "testdata/mathing.HC".to_string(),
            "testdata/small.HC".to_string(),
        ]
    };
    let mut sesh = ParseSess::new(files);
    dbg!(&sesh);
    sesh.mk_asteez();
    let _ = sesh.build_symbol_tables();

    // let context = CodegenContext::create();
    // let mut codegen =
    //     Codegen::new(&context, &"test_module".to_string()).expect("Failed to create Codegen");
    //
    // let hir_modules = HIRContext::make(sesh.modules);
    // codegen.compile(&hir_modules).expect("Compilation failed");
    // codegen.dump_ir();
    // // may be bad practice but patience kids
    // codegen.emit_object_file("target/main.o");
}
