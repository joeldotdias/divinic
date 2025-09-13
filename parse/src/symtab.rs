use std::collections::HashMap;

use ast::ast::{Label, Type};

#[derive(Debug, Clone)]
pub struct SymbolTable {
    pub module_scopes: Vec<ModuleScope>,
}

#[derive(Debug, Clone)]
pub struct ModuleScope {
    pub local_symbols: Scope,
    pub resolved_symbols: Scope,
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub symbols: HashMap<Label, Type>,
}

impl Scope {
    pub fn new() -> Self {
        Scope {
            symbols: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: Label, ty: Type) {
        self.symbols.insert(name, ty);
    }

    pub fn lookup(&self, name: &Label) -> Option<&Type> {
        self.symbols.get(name)
    }

    pub fn merge(&mut self, other: &Scope) {
        for (name, ty) in &other.symbols {
            self.symbols.insert(name.clone(), ty.clone());
        }
    }
}
