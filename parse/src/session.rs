use std::{fs, path::PathBuf};

use ast::ast::{Declaration, Label, Module, Type};

use crate::{
    AnnotatedModule,
    lexer::lex_token_trees,
    parser::Parser,
    symtab::{ModuleScope, Scope, SymbolTable},
};

#[derive(Default, Debug)]
pub struct ParseSess {
    pub source_files: Vec<SourceFile>,
    pub curr: u16,
    pub modules: Vec<Module>,

    pub sym_tab: Option<SymbolTable>,
}

impl ParseSess {
    pub fn new(fnames: Vec<String>) -> Self {
        let source_files = fnames.into_iter().map(|f| SourceFile::from(f)).collect();

        ParseSess {
            source_files,
            curr: 0,
            modules: Vec::new(),
            sym_tab: None,
        }
    }

    pub fn mk_asteez(&mut self) {
        for (i, f) in self.source_files.iter().enumerate() {
            self.curr = i as u16;

            let stream = match lex_token_trees(&self, &f.src) {
                Ok(ts) => ts,
                Err(errs) => {
                    for e in errs {
                        let (filename, source) = self.src_file(e.loc.fid as usize);
                        e.report(filename.to_str().unwrap(), source);
                    }
                    continue;
                }
            };

            // let mut cursor = TokenCursor::new(stream);
            // loop {
            //     let token = cursor.next();
            //     if matches!(token.kind, TokenKind::Eof) {
            //         break;
            //     }
            //     println!("{:?}", token);
            // }

            let mut parser = Parser::new(&self, stream);
            match parser.parse_module() {
                Ok(parsed) => {
                    for e in parser.errs {
                        let (filename, source) = self.src_file(e.loc.fid as usize);
                        e.report(filename.to_str().unwrap(), source);
                    }
                    self.modules.push(parsed);
                }
                Err(err) => {
                    let mut errs = parser.errs;
                    errs.push(err);
                    for e in errs {
                        let (filename, source) = self.src_file(e.loc.fid as usize);
                        e.report(filename.to_str().unwrap(), source);
                    }
                    return;
                }
            };
        }
    }

    pub fn build_symbol_tables(&mut self) -> Result<(), String> {
        let mut sym_tab = SymbolTable {
            module_scopes: Vec::new(),
        };

        for (_midx, module) in self.modules.iter().enumerate() {
            let mut local_scope = Scope::new();

            for decl in &module.decls {
                match decl {
                    Declaration::Func {
                        name,
                        ret_ty,
                        params,
                        ..
                    } => {
                        let params_tys: Vec<Type> = params.iter().map(|p| p.ty.clone()).collect();

                        let func_ty = Type::Function {
                            params: params_tys,
                            ret: Box::new(ret_ty.clone()),
                            varargs: false,
                        };

                        local_scope.insert(name.clone(), func_ty);
                    }
                    _ => {}
                }
            }

            sym_tab.module_scopes.push(ModuleScope {
                local_symbols: local_scope,
                resolved_symbols: Scope::new(),
            });
        }

        // resolving
        for (midx, module) in self.modules.iter().enumerate() {
            let mut resolved = sym_tab.module_scopes[midx].local_symbols.clone();

            for decl in module.decls.clone() {
                if let Declaration::Include { name, .. } = decl {
                    if let Some(included_idx) = self.find_module_by_name(&name) {
                        let included_scope = &sym_tab.module_scopes[included_idx].local_symbols;
                        resolved.merge(included_scope);
                    } else {
                        panic!("Didn't find {}", name);
                        // handle err
                    }
                }
            }

            sym_tab.module_scopes[midx].resolved_symbols = resolved;
        }

        self.sym_tab = Some(sym_tab);

        Ok(())
    }

    fn find_module_by_name(&self, name: &str) -> Option<usize> {
        for (idx, source_file) in self.source_files.iter().enumerate() {
            let sbpt = source_file.name.to_str();
            if sbpt == Some(name) {
                return Some(idx);
            }
        }
        None
    }

    pub fn lookup_sym(&self, mod_idx: usize, name: &Label) -> Option<&Type> {
        if let Some(ref sym_tab) = self.sym_tab {
            sym_tab.module_scopes[mod_idx].resolved_symbols.lookup(name)
        } else {
            None
        }
    }

    pub fn mk_annoted_modules(&self) -> Vec<AnnotatedModule> {
        if self.sym_tab.is_none() {
            panic!("No symbol table whatttttt");
        }

        let sym_tab = self.sym_tab.as_ref().unwrap();
        let mut amods = Vec::new();

        for (idx, module) in self.modules.iter().enumerate() {
            let name = self.source_files[idx]
                .name
                .to_string_lossy()
                .into_owned()
                .into();

            let resolved_symbols = sym_tab.module_scopes[idx].resolved_symbols.symbols.clone();

            amods.push(AnnotatedModule {
                name,
                ast: module.clone(),
                resolved_symbols,
            });
        }

        amods
    }

    pub fn src_file(&self, idx: usize) -> (PathBuf, &str) {
        let f = &self.source_files[idx];
        (f.name.clone(), f.src.as_str())
    }
}

#[derive(Debug)]
pub struct SourceFile {
    pub name: PathBuf,
    pub src: String,
    pub line_starts: Vec<usize>,
}

impl From<String> for SourceFile {
    fn from(value: String) -> Self {
        let path = PathBuf::from(value);
        let src = fs::read_to_string(&path).expect("Couldn't read file");

        SourceFile {
            name: path,
            src,
            line_starts: Vec::new(),
        }
    }
}
