use std::collections::HashMap;

use InbuiltType::*; // don't mind me for a minute here
use ast::ast::{
    BinaryOp, Case, Constant, Declaration, Enumerator, Expr, InbuiltType, Label, Module, Param,
    Stmt, Type, UnaryOp,
};
use ast::span::{DUMMY_SPAN, Span};

use ecow::EcoString;

#[derive(Clone, Debug, PartialEq)]
pub struct HIRModule {
    pub decls: Vec<HIRDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HIRDeclaration {
    Var {
        span: Span,
        name: Label,
        ty: Type,
        init: Option<HIRExpr>,
    },
    Func {
        span: Span,
        name: Label,
        ret_ty: Type,
        params: Vec<HIRParam>,
        body: HIRStmt,
    },
    Struct {
        span: Span,
        name: Option<Label>,
        fields: Vec<HIRDeclaration>,
    },
    Enum {
        span: Span,
        name: Option<Label>,
        variants: Vec<HIREnumerator>,
    },
    Include {
        name: Label,
        as_name: Option<EcoString>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct HIRParam {
    pub span: Span,
    pub name: Label,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HIREnumerator {
    pub span: Span,
    pub name: Label,
    pub value: Option<HIRExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HIRExpr {
    Ident {
        span: Span,
        name: Label,
        ty: Type,
    },
    Constant {
        span: Span,
        value: Constant,
        ty: Type,
    },
    Assign {
        span: Span,
        op: BinaryOp,
        lhs: Box<HIRExpr>,
        rhs: Box<HIRExpr>,
        ty: Type,
    },
    Binary {
        span: Span,
        op: BinaryOp,
        lhs: Box<HIRExpr>,
        rhs: Box<HIRExpr>,
        ty: Type,
    },
    Unary {
        span: Span,
        op: UnaryOp,
        expr: Box<HIRExpr>,
        ty: Type,
    },
    Call {
        span: Span,
        func: Label,
        args: Vec<HIRExpr>,
        ty: Type,
    },
    Member {
        span: Span,
        base: Box<HIRExpr>,
        field: Label,
        arrow: bool,
        ty: Type,
    },
    Index {
        span: Span,
        base: Box<HIRExpr>,
        index: Box<HIRExpr>,
        ty: Type,
    },
    Conditional {
        span: Span,
        cond: Box<HIRExpr>,
        then_expr: Box<HIRExpr>,
        else_expr: Box<HIRExpr>,
        ty: Type,
    },
    Cast {
        span: Span,
        ty: Type,
        expr: Box<HIRExpr>,
    },
    Paren {
        span: Span,
        expr: Box<HIRExpr>,
        ty: Type,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum HIRStmt {
    Expr {
        span: Span,
        expr: HIRExpr,
        ty: Type,
    },
    Block {
        span: Span,
        stmts: Vec<HIRStmt>,
    },
    VarDecl {
        span: Span,
        name: Label,
        ty: Type,
        init: Option<HIRExpr>,
    },
    If {
        span: Span,
        cond_then_ladder: Vec<(HIRExpr, HIRStmt)>,
        else_branch: Option<Box<HIRStmt>>,
    },
    Switch {
        span: Span,
        expr: HIRExpr,
        cases: Vec<HIRCase>,
        nobounds: bool,
    },
    While {
        span: Span,
        cond: HIRExpr,
        body: Box<HIRStmt>,
    },
    For {
        span: Span,
        init: Option<Box<HIRStmt>>,
        cond: Option<HIRExpr>,
        step: Option<HIRExpr>,
        body: Box<HIRStmt>,
    },
    Return {
        span: Span,
        expr: Option<HIRExpr>,
    },
    Break {
        span: Span,
    },
    Continue {
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct HIRCase {
    pub values: Option<Vec<Constant>>,
    pub body: HIRStmt,
}

type SymTable = HashMap<EcoString, SymEntry>;

#[derive(Clone, Debug)]
struct SymEntry {
    ty: Type,
}

pub struct HIRContext;

impl HIRContext {
    pub fn make(modules: Vec<Module>) -> Vec<HIRModule> {
        let mut hir_modules: Vec<HIRModule> = modules
            .into_iter()
            .map(Self::ast_module_to_hmodule)
            .collect();
        Self::infer_types(&mut hir_modules);
        for module in &mut hir_modules {
            Self::ensure_returns(module);
        }
        hir_modules
    }

    fn ast_module_to_hmodule(module: Module) -> HIRModule {
        HIRModule {
            decls: module
                .decls
                .into_iter()
                .map(Self::ast_decl_to_hir)
                .collect(),
        }
    }

    fn ast_decl_to_hir(decl: Declaration) -> HIRDeclaration {
        match decl {
            Declaration::Var {
                span,
                name,
                ty,
                init,
            } => HIRDeclaration::Var {
                span,
                name,
                ty: ty.clone(),
                init: init.map(Self::ast_expr_to_hir),
            },
            Declaration::Func {
                span,
                name,
                ret_ty,
                params,
                body,
            } => HIRDeclaration::Func {
                span,
                name,
                ret_ty: ret_ty.clone(),
                params: params.into_iter().map(Self::ast_param_to_hir).collect(),
                body: Self::ast_stmt_to_hir(body),
            },
            Declaration::Struct { span, name, fields } => HIRDeclaration::Struct {
                span,
                name,
                fields: fields.into_iter().map(Self::ast_decl_to_hir).collect(),
            },
            Declaration::Enum {
                span,
                name,
                variants,
            } => HIRDeclaration::Enum {
                span,
                name,
                variants: variants.into_iter().map(Self::ast_enum_to_hir).collect(),
            },
            Declaration::Include { name, as_name } => HIRDeclaration::Include { name, as_name },
        }
    }

    fn ast_param_to_hir(param: Param) -> HIRParam {
        HIRParam {
            span: param.span,
            name: param.name,
            ty: param.ty,
        }
    }

    fn ast_enum_to_hir(enumerator: Enumerator) -> HIREnumerator {
        HIREnumerator {
            span: enumerator.span,
            name: enumerator.name,
            value: enumerator.value.map(Self::ast_expr_to_hir),
        }
    }

    fn ast_stmt_to_hir(stmt: Stmt) -> HIRStmt {
        match stmt {
            Stmt::Expr { span, expr } => HIRStmt::Expr {
                span,
                expr: Self::ast_expr_to_hir(expr),
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Stmt::Block { span, stmts } => HIRStmt::Block {
                span,
                stmts: stmts.into_iter().map(Self::ast_stmt_to_hir).collect(),
            },
            Stmt::VarDecl {
                span,
                name,
                ty,
                init,
            } => HIRStmt::VarDecl {
                span,
                name,
                ty,
                init: init.map(Self::ast_expr_to_hir),
            },
            Stmt::If {
                span,
                cond_then_ladder,
                else_branch,
            } => HIRStmt::If {
                span,
                cond_then_ladder: cond_then_ladder
                    .into_iter()
                    .map(|(c, s)| (Self::ast_expr_to_hir(c), Self::ast_stmt_to_hir(s)))
                    .collect(),
                else_branch: else_branch.map(|b| Box::new(Self::ast_stmt_to_hir(*b))),
            },
            Stmt::Switch {
                span,
                expr,
                cases,
                nobounds,
            } => HIRStmt::Switch {
                span,
                expr: Self::ast_expr_to_hir(expr),
                cases: cases.into_iter().map(Self::ast_case_to_hir).collect(),
                nobounds,
            },
            Stmt::While { span, cond, body } => HIRStmt::While {
                span,
                cond: Self::ast_expr_to_hir(cond),
                body: Box::new(Self::ast_stmt_to_hir(*body)),
            },
            Stmt::For {
                span,
                init,
                cond,
                step,
                body,
            } => HIRStmt::For {
                span,
                init: init.map(|s| Box::new(Self::ast_stmt_to_hir(*s))),
                cond: cond.map(Self::ast_expr_to_hir),
                step: step.map(Self::ast_expr_to_hir),
                body: Box::new(Self::ast_stmt_to_hir(*body)),
            },
            Stmt::Return { span, expr } => HIRStmt::Return {
                span,
                expr: expr.map(Self::ast_expr_to_hir),
            },
            Stmt::Break { span } => HIRStmt::Break { span },
            Stmt::Continue { span } => HIRStmt::Continue { span },
        }
    }

    fn ast_case_to_hir(case: Case) -> HIRCase {
        HIRCase {
            values: case.values,
            body: Self::ast_stmt_to_hir(case.body),
        }
    }

    fn ast_expr_to_hir(expr: Expr) -> HIRExpr {
        match expr {
            Expr::Ident { span, name } => HIRExpr::Ident {
                span,
                name,
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Constant { span, value } => HIRExpr::Constant {
                span,
                value,
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Assign { span, op, lhs, rhs } => HIRExpr::Assign {
                span,
                op,
                lhs: Box::new(Self::ast_expr_to_hir(*lhs)),
                rhs: Box::new(Self::ast_expr_to_hir(*rhs)),
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Binary { span, op, lhs, rhs } => HIRExpr::Binary {
                span,
                op,
                lhs: Box::new(Self::ast_expr_to_hir(*lhs)),
                rhs: Box::new(Self::ast_expr_to_hir(*rhs)),
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Unary { span, op, expr } => HIRExpr::Unary {
                span,
                op,
                expr: Box::new(Self::ast_expr_to_hir(*expr)),
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Call { span, func, args } => HIRExpr::Call {
                span,
                func,
                args: args.into_iter().map(Self::ast_expr_to_hir).collect(),
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Member {
                span,
                base,
                field,
                arrow,
            } => HIRExpr::Member {
                span,
                base: Box::new(Self::ast_expr_to_hir(*base)),
                field,
                arrow,
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Index { span, base, index } => HIRExpr::Index {
                span,
                base: Box::new(Self::ast_expr_to_hir(*base)),
                index: Box::new(Self::ast_expr_to_hir(*index)),
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Conditional {
                span,
                cond,
                then_expr,
                else_expr,
            } => HIRExpr::Conditional {
                span,
                cond: Box::new(Self::ast_expr_to_hir(*cond)),
                then_expr: Box::new(Self::ast_expr_to_hir(*then_expr)),
                else_expr: Box::new(Self::ast_expr_to_hir(*else_expr)),
                ty: Type::Inbuilt(InbuiltType::U0),
            },
            Expr::Cast { span, ty, expr } => HIRExpr::Cast {
                span,
                ty: *ty,
                expr: Box::new(Self::ast_expr_to_hir(*expr)),
            },
            Expr::Paren { span, expr } => HIRExpr::Paren {
                span,
                expr: Box::new(Self::ast_expr_to_hir(*expr)),
                ty: Type::Inbuilt(InbuiltType::U0),
            },
        }
    }

    fn infer_types(modules: &mut [HIRModule]) {
        for module in modules {
            let mut globals = SymTable::new();
            for decl in &module.decls {
                if let HIRDeclaration::Var { name, ty, .. } = decl {
                    globals.insert(name.clone(), SymEntry { ty: ty.clone() });
                }
            }
            for decl in &mut module.decls {
                Self::infer_decl_types(decl, &mut globals);
            }
        }
    }

    fn infer_decl_types(decl: &mut HIRDeclaration, globals: &mut SymTable) {
        match decl {
            HIRDeclaration::Var { init, .. } => {
                if let Some(expr) = init {
                    let mut table = globals.clone();
                    Self::infer_expr(expr, &mut table, globals);
                }
            }
            HIRDeclaration::Func {
                params,
                body,
                ret_ty,
                ..
            } => {
                let mut locals: SymTable = params
                    .iter()
                    .map(|p| (p.name.clone(), SymEntry { ty: p.ty.clone() }))
                    .collect();
                Self::infer_stmt(body, &mut locals, globals, Some(ret_ty));
            }
            HIRDeclaration::Struct { fields, .. } => {
                for f in fields {
                    Self::infer_decl_types(f, globals);
                }
            }
            HIRDeclaration::Enum { variants, .. } => {
                for v in variants {
                    if let Some(expr) = &mut v.value {
                        Self::infer_expr(expr, &mut globals.clone(), globals);
                    }
                }
            }
            HIRDeclaration::Include { .. } => {}
        }
    }

    fn ensure_returns(module: &mut HIRModule) {
        for decl in &mut module.decls {
            if let HIRDeclaration::Func { ret_ty, body, .. } = decl {
                if *ret_ty == Type::Inbuilt(InbuiltType::U0) {
                    Self::append_void_return(body);
                }
            }
        }
    }

    fn append_void_return(stmt: &mut HIRStmt) {
        match stmt {
            HIRStmt::Block { stmts, .. } => {
                if let Some(last) = stmts.last_mut() {
                    Self::append_void_return(last);
                } else {
                    stmts.push(HIRStmt::Return {
                        span: DUMMY_SPAN,
                        expr: None,
                    });
                }
            }
            HIRStmt::Return { .. } => {}
            _ => {
                *stmt = HIRStmt::Block {
                    span: DUMMY_SPAN,
                    stmts: vec![
                        stmt.clone(),
                        HIRStmt::Return {
                            span: DUMMY_SPAN,
                            expr: None,
                        },
                    ],
                };
            }
        }
    }

    fn infer_stmt(
        stmt: &mut HIRStmt,
        locals: &mut SymTable,
        globals: &SymTable,
        expected_ret_ty: Option<&Type>,
    ) {
        match stmt {
            HIRStmt::Expr { expr, ty, .. } => {
                Self::infer_expr(expr, locals, globals);
                *ty = expr.ty();
            }
            HIRStmt::Block { stmts, .. } => {
                for s in stmts {
                    Self::infer_stmt(s, locals, globals, expected_ret_ty);
                }
            }
            HIRStmt::VarDecl { name, ty, init, .. } => {
                if let Some(e) = init {
                    Self::infer_expr(e, locals, globals);
                }
                locals.insert(name.clone(), SymEntry { ty: ty.clone() });
            }
            HIRStmt::If {
                cond_then_ladder,
                else_branch,
                ..
            } => {
                for (c, s) in cond_then_ladder {
                    Self::infer_expr(c, locals, globals);
                    Self::infer_stmt(s, locals, globals, expected_ret_ty);
                }
                if let Some(e) = else_branch {
                    Self::infer_stmt(e, locals, globals, expected_ret_ty);
                }
            }
            HIRStmt::Switch { expr, cases, .. } => {
                Self::infer_expr(expr, locals, globals);
                for case in cases {
                    Self::infer_stmt(&mut case.body, locals, globals, expected_ret_ty);
                }
            }
            HIRStmt::While { cond, body, .. } => {
                Self::infer_expr(cond, locals, globals);
                Self::infer_stmt(body, locals, globals, expected_ret_ty);
            }
            HIRStmt::For {
                init,
                cond,
                step,
                body,
                ..
            } => {
                if let Some(i) = init {
                    Self::infer_stmt(i, locals, globals, expected_ret_ty);
                }
                if let Some(c) = cond {
                    Self::infer_expr(c, locals, globals);
                }
                if let Some(s) = step {
                    Self::infer_expr(s, locals, globals);
                }
                Self::infer_stmt(body, locals, globals, expected_ret_ty);
            }
            HIRStmt::Return { expr, .. } => {
                if let Some(e) = expr {
                    Self::infer_expr(e, locals, globals);
                    if let Some(expected_ty) = expected_ret_ty {
                        if Self::is_type_compatible(&e.ty(), expected_ty) {
                            *e.ty_mut() = expected_ty.clone();
                        }
                    }
                }
            }
            HIRStmt::Break { .. } | HIRStmt::Continue { .. } => {}
        }
    }

    fn infer_expr(expr: &mut HIRExpr, locals: &mut SymTable, globals: &SymTable) {
        match expr {
            HIRExpr::Ident { name, ty, .. } => {
                if let Some(entry) = locals.get(name).or_else(|| globals.get(name)) {
                    *ty = entry.ty.clone();
                }
            }
            HIRExpr::Constant { value, ty, .. } => {
                *ty = match value {
                    Constant::Int(_) => Type::Inbuilt(InbuiltType::I64),
                    Constant::UInt(_) => Type::Inbuilt(InbuiltType::U64),
                    Constant::Float(_) => Type::Inbuilt(InbuiltType::F64),
                    Constant::Bool(_) => Type::Inbuilt(InbuiltType::Bool),
                    Constant::Char(_) => Type::Inbuilt(InbuiltType::U8),
                    _ => Type::Inbuilt(InbuiltType::U0),
                };
            }
            HIRExpr::Assign { lhs, rhs, ty, .. } => {
                Self::infer_expr(lhs, locals, globals);
                Self::infer_expr(rhs, locals, globals);
                *ty = lhs.ty();
            }
            HIRExpr::Binary { lhs, rhs, ty, .. } => {
                Self::infer_expr(lhs, locals, globals);
                Self::infer_expr(rhs, locals, globals);
                *ty = Self::unify_types(lhs.ty(), rhs.ty());
            }
            HIRExpr::Unary { expr: e, ty, .. } => {
                Self::infer_expr(e, locals, globals);
                *ty = e.ty();
            }
            HIRExpr::Call { func, args, ty, .. } => {
                for a in args {
                    Self::infer_expr(a, locals, globals);
                }
                if let Some(entry) = locals.get(func).or_else(|| globals.get(func)) {
                    *ty = entry.ty.clone();
                } else {
                    *ty = Type::Inbuilt(InbuiltType::U0);
                }
            }
            HIRExpr::Member { base, ty, .. } => {
                Self::infer_expr(base, locals, globals);
                *ty = base.ty();
            }
            HIRExpr::Index {
                base, index, ty, ..
            } => {
                Self::infer_expr(base, locals, globals);
                Self::infer_expr(index, locals, globals);
                *ty = base.ty();
            }
            HIRExpr::Conditional {
                cond,
                then_expr,
                else_expr,
                ty,
                ..
            } => {
                Self::infer_expr(cond, locals, globals);
                Self::infer_expr(then_expr, locals, globals);
                Self::infer_expr(else_expr, locals, globals);
                *ty = Self::unify_types(then_expr.ty(), else_expr.ty());
            }
            HIRExpr::Cast { expr: e, .. } => {
                Self::infer_expr(e, locals, globals);
            }
            HIRExpr::Paren { expr: e, ty, .. } => {
                Self::infer_expr(e, locals, globals);
                *ty = e.ty();
            }
        }
    }

    fn unify_types(lhs: Type, rhs: Type) -> Type {
        match (lhs, rhs) {
            (Type::Inbuilt(InbuiltType::F64), _) | (_, Type::Inbuilt(InbuiltType::F64)) => {
                Type::Inbuilt(InbuiltType::F64)
            }
            (Type::Inbuilt(InbuiltType::I64), _) | (_, Type::Inbuilt(InbuiltType::I64)) => {
                Type::Inbuilt(InbuiltType::I64)
            }
            (Type::Inbuilt(InbuiltType::I32), _) | (_, Type::Inbuilt(InbuiltType::I32)) => {
                Type::Inbuilt(InbuiltType::I32)
            }
            (Type::Inbuilt(InbuiltType::I8), Type::Inbuilt(InbuiltType::U8))
            | (Type::Inbuilt(InbuiltType::U8), Type::Inbuilt(InbuiltType::I8)) => {
                Type::Inbuilt(InbuiltType::I8)
            }
            (t, _) => t,
        }
    }

    // sybau
    fn is_type_compatible(expr_ty: &Type, ret_ty: &Type) -> bool {
        match (expr_ty, ret_ty) {
            (a, b) if a == b => true,

            (Type::Inbuilt(I8), Type::Inbuilt(I16))
            | (Type::Inbuilt(I8), Type::Inbuilt(I32))
            | (Type::Inbuilt(I8), Type::Inbuilt(I64)) => true,

            (Type::Inbuilt(U8), Type::Inbuilt(I16))
            | (Type::Inbuilt(U8), Type::Inbuilt(I32))
            | (Type::Inbuilt(U8), Type::Inbuilt(I64))
            | (Type::Inbuilt(U8), Type::Inbuilt(U16))
            | (Type::Inbuilt(U8), Type::Inbuilt(U32))
            | (Type::Inbuilt(U8), Type::Inbuilt(U64)) => true,

            (Type::Inbuilt(I16), Type::Inbuilt(I32)) | (Type::Inbuilt(I16), Type::Inbuilt(I64)) => {
                true
            }

            (Type::Inbuilt(U16), Type::Inbuilt(I32))
            | (Type::Inbuilt(U16), Type::Inbuilt(I64))
            | (Type::Inbuilt(U16), Type::Inbuilt(U32))
            | (Type::Inbuilt(U16), Type::Inbuilt(U64)) => true,

            (Type::Inbuilt(I32), Type::Inbuilt(I64)) => true,

            (Type::Inbuilt(U32), Type::Inbuilt(I64)) | (Type::Inbuilt(U32), Type::Inbuilt(U64)) => {
                true
            }

            (Type::Inbuilt(I8), Type::Inbuilt(F64))
            | (Type::Inbuilt(I16), Type::Inbuilt(F64))
            | (Type::Inbuilt(I32), Type::Inbuilt(F64))
            | (Type::Inbuilt(I64), Type::Inbuilt(F64))
            | (Type::Inbuilt(U8), Type::Inbuilt(F64))
            | (Type::Inbuilt(U16), Type::Inbuilt(F64))
            | (Type::Inbuilt(U32), Type::Inbuilt(F64))
            | (Type::Inbuilt(U64), Type::Inbuilt(F64)) => true,

            _ => false,
        }
    }
}

// this is so cool and big brain

pub trait HIRExprGives {
    fn ty(&self) -> Type;
    fn ty_mut(&mut self) -> &mut Type;
    fn span(&self) -> Span;
}

impl HIRExprGives for HIRExpr {
    fn ty(&self) -> Type {
        match self {
            HIRExpr::Ident { ty, .. }
            | HIRExpr::Constant { ty, .. }
            | HIRExpr::Assign { ty, .. }
            | HIRExpr::Binary { ty, .. }
            | HIRExpr::Unary { ty, .. }
            | HIRExpr::Call { ty, .. }
            | HIRExpr::Member { ty, .. }
            | HIRExpr::Index { ty, .. }
            | HIRExpr::Conditional { ty, .. }
            | HIRExpr::Cast { ty, .. }
            | HIRExpr::Paren { ty, .. } => ty.clone(),
        }
    }

    fn ty_mut(&mut self) -> &mut Type {
        match self {
            HIRExpr::Ident { ty, .. }
            | HIRExpr::Constant { ty, .. }
            | HIRExpr::Assign { ty, .. }
            | HIRExpr::Binary { ty, .. }
            | HIRExpr::Unary { ty, .. }
            | HIRExpr::Call { ty, .. }
            | HIRExpr::Member { ty, .. }
            | HIRExpr::Index { ty, .. }
            | HIRExpr::Conditional { ty, .. }
            | HIRExpr::Cast { ty, .. }
            | HIRExpr::Paren { ty, .. } => ty,
        }
    }

    fn span(&self) -> Span {
        match self {
            HIRExpr::Ident { span, .. }
            | HIRExpr::Constant { span, .. }
            | HIRExpr::Assign { span, .. }
            | HIRExpr::Binary { span, .. }
            | HIRExpr::Unary { span, .. }
            | HIRExpr::Call { span, .. }
            | HIRExpr::Member { span, .. }
            | HIRExpr::Index { span, .. }
            | HIRExpr::Conditional { span, .. }
            | HIRExpr::Cast { span, .. }
            | HIRExpr::Paren { span, .. } => *span,
        }
    }
}
