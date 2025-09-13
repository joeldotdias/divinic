use inkwell::targets::TargetMachine;
use std::process::Command;
use std::path::{Path, PathBuf};

use crate::codegen::Codegen;

impl<'ctx> Codegen<'ctx> {
    pub fn emit_object_file(&self, filename: &str) {
        use inkwell::targets::{FileType, InitializationConfig, Target};

        // Initialize LLVM targets
        Target::initialize_all(&InitializationConfig::default());

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).expect("Could not get target from triple");

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                inkwell::OptimizationLevel::None,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("Failed to create target machine");

        // Write the object file
        let object_path = Path::new(filename);
        target_machine
            .write_to_file(&self.module, FileType::Object, object_path)
            .expect("Could not write object file");

        // Generate binary in the same path
        let binary_path = object_path.with_extension(""); // remove .o or any extension

        let status = Command::new("cc") // or "gcc" / "clang"
            .arg(object_path)
            .arg("-o")
            .arg(&binary_path)
            .status()
            .expect("Failed to run system linker");

        if !status.success() {
            panic!("Linking failed");
        }

        println!(
            "Generated object file: {:?} and binary: {:?}",
            object_path, binary_path
        );
    }
}

