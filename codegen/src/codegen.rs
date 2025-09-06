// use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    // values::{FunctionValue, PointerValue},
};

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    // functions: HashMap<String, FunctionValue<'ctx>>,
    // variables: HashMap<String, PointerValue<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context, name: &str) -> Self {
        let module = context.create_module(name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
        }
    }
}
