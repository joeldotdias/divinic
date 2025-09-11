use inkwell::targets::TargetMachine;

use crate::codegen::Codegen;

impl<'ctx> Codegen<'ctx> {
    pub fn emit_object_file(&self, filename: &str) {
        use inkwell::targets::{FileType, InitializationConfig, Target};

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

        target_machine
            .write_to_file(
                &self.module,
                FileType::Object,
                std::path::Path::new(filename),
            )
            .expect("Could not write object file");
    }
}
