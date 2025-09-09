use std::collections::HashMap;

use ast::ast::{
    BinaryOp, Case, Constant, Declaration, Enumerator, Expr, InbuiltType, Label, Module, NodeId,
    Param, Stmt, Type, UnaryOp,
};
use ast::span::Span;

use ecow::EcoString;

#[derive(Clone, Debug, PartialEq)]
pub struct HIRModule {
    pub decls: Vec<HIRDeclaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HIRDeclaration {
    Var {
        id: NodeId,
        span: Span,
        name: Label,
        ty: Type,
        init: Option<HIRExpr>,
    },
    Func {
        id: NodeId,
        span: Span,
        name: Label,
        ret_ty: Type,
        params: Vec<HIRParam>,
        body: HIRStmt,
    },
    Struct {
        id: NodeId,
        span: Span,
        name: Option<Label>,
        fields: Vec<HIRDeclaration>,
    },
    Enum {
        id: NodeId,
        span: Span,
        name: Option<Label>,
        variants: Vec<HIREnumerator>,
    },
    Include {
        id: NodeId,
        name: Label,
        as_name: Option<EcoString>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct HIRParam {
    pub id: NodeId,
    pub span: Span,
    pub name: Label,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HIREnumerator {
    pub id: NodeId,
    pub span: Span,
    pub name: Label,
    pub value: Option<HIRExpr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum HIRExpr {
    Ident {
        id: NodeId,
        span: Span,
        name: Label,
        ty: Type,
    },
    Constant {
        id: NodeId,
        span: Span,
        value: Constant,
        ty: Type,
    },
    Assign {
        id: NodeId,
        span: Span,
        lhs: Box<HIRExpr>,
        rhs: Box<HIRExpr>,
        ty: Type,
    },
    Binary {
        id: NodeId,
        span: Span,
        op: BinaryOp,
        lhs: Box<HIRExpr>,
        rhs: Box<HIRExpr>,
        ty: Type,
    },
    Unary {
        id: NodeId,
        span: Span,
        op: UnaryOp,
        expr: Box<HIRExpr>,
        ty: Type,
    },
    Call {
        id: NodeId,
        span: Span,
        func: Box<HIRExpr>,
        args: Vec<HIRExpr>,
        ty: Type,
    },
    Member {
        id: NodeId,
        span: Span,
        base: Box<HIRExpr>,
        field: Label,
        arrow: bool,
        ty: Type,
    },
    Index {
        id: NodeId,
        span: Span,
        base: Box<HIRExpr>,
        index: Box<HIRExpr>,
        ty: Type,
    },
    Conditional {
        id: NodeId,
        span: Span,
        cond: Box<HIRExpr>,
        then_expr: Box<HIRExpr>,
        else_expr: Box<HIRExpr>,
        ty: Type,
    },
    Cast {
        id: NodeId,
        span: Span,
        ty: Type,
        expr: Box<HIRExpr>,
    },
    Paren {
        id: NodeId,
        span: Span,
        expr: Box<HIRExpr>,
        ty: Type,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum HIRStmt {
    Expr {
        id: NodeId,
        span: Span,
        expr: HIRExpr,
        ty: Type,
    },
    Block {
        id: NodeId,
        span: Span,
        stmts: Vec<HIRStmt>,
    },
    VarDecl {
        id: NodeId,
        span: Span,
        name: Label,
        ty: Type,
        init: Option<HIRExpr>,
    },
    If {
        id: NodeId,
        span: Span,
        cond: HIRExpr,
        then_branch: Box<HIRStmt>,
        ladder: Vec<(HIRExpr, HIRStmt)>,
        else_branch: Option<Box<HIRStmt>>,
    },
    Switch {
        id: NodeId,
        span: Span,
        expr: HIRExpr,
        cases: Vec<HIRCase>,
        nobounds: bool,
    },
    While {
        id: NodeId,
        span: Span,
        cond: HIRExpr,
        body: Box<HIRStmt>,
    },
    For {
        id: NodeId,
        span: Span,
        init: Option<Box<HIRStmt>>,
        cond: Option<HIRExpr>,
        step: Option<HIRExpr>,
        body: Box<HIRStmt>,
    },
    Return {
        id: NodeId,
        span: Span,
        expr: Option<HIRExpr>,
    },
    Break {
        id: NodeId,
        span: Span,
    },
    Continue {
        id: NodeId,
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct HIRCase {
    pub values: Option<Vec<Constant>>,
    pub body: HIRStmt,
}

#[derive(Clone, Debug)]
struct SymEntry {
    ty: Type,
}

type SymTable = HashMap<EcoString, SymEntry>;

pub fn ast_to_hir(modules: Vec<Module>) -> Vec<HIRModule> {
    modules.into_iter().map(ast_module_to_hmodule).collect()
}

fn ast_module_to_hmodule(module: Module) -> HIRModule {
    HIRModule {
        decls: module.decls.into_iter().map(ast_decl_to_hir).collect(),
    }
}

fn ast_decl_to_hir(decl: Declaration) -> HIRDeclaration {
    match decl {
        Declaration::Var {
            id,
            span,
            name,
            ty,
            init,
        } => HIRDeclaration::Var {
            id,
            span,
            name,
            ty: ty.clone(), // will infer later
            init: init.map(ast_expr_to_hir),
        },
        Declaration::Func {
            id,
            span,
            name,
            ret_ty,
            params,
            body,
        } => HIRDeclaration::Func {
            id,
            span,
            name,
            ret_ty: ret_ty.clone(),
            params: params.into_iter().map(ast_param_to_hir).collect(),
            body: ast_stmt_to_hir(body),
        },
        Declaration::Struct {
            id,
            span,
            name,
            fields,
        } => HIRDeclaration::Struct {
            id,
            span,
            name,
            fields: fields.into_iter().map(ast_decl_to_hir).collect(),
        },
        Declaration::Enum {
            id,
            span,
            name,
            variants,
        } => HIRDeclaration::Enum {
            id,
            span,
            name,
            variants: variants.into_iter().map(ast_enum_to_hir).collect(),
        },
        Declaration::Include { id, name, as_name } => HIRDeclaration::Include { id, name, as_name },
    }
}

fn ast_param_to_hir(param: Param) -> HIRParam {
    HIRParam {
        id: param.id,
        span: param.span,
        name: param.name,
        ty: param.ty,
    }
}

fn ast_enum_to_hir(enumerator: Enumerator) -> HIREnumerator {
    HIREnumerator {
        id: enumerator.id,
        span: enumerator.span,
        name: enumerator.name,
        value: enumerator.value.map(ast_expr_to_hir),
    }
}

fn ast_stmt_to_hir(stmt: Stmt) -> HIRStmt {
    match stmt {
        Stmt::Expr { id, span, expr } => HIRStmt::Expr {
            id,
            span,
            expr: ast_expr_to_hir(expr),
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Stmt::Block { id, span, stmts } => HIRStmt::Block {
            id,
            span,
            stmts: stmts.into_iter().map(ast_stmt_to_hir).collect(),
        },
        Stmt::VarDecl {
            id,
            span,
            name,
            ty,
            init,
        } => HIRStmt::VarDecl {
            id,
            span,
            name,
            ty,
            init: init.map(ast_expr_to_hir),
        },
        Stmt::If {
            id,
            span,
            cond,
            then_branch,
            ladder,
            else_branch,
        } => HIRStmt::If {
            id,
            span,
            cond: ast_expr_to_hir(cond),
            then_branch: Box::new(ast_stmt_to_hir(*then_branch)),
            ladder: ladder
                .into_iter()
                .map(|(c, s)| (ast_expr_to_hir(c), ast_stmt_to_hir(s)))
                .collect(),
            else_branch: else_branch.map(|b| Box::new(ast_stmt_to_hir(*b))),
        },
        Stmt::Switch {
            id,
            span,
            expr,
            cases,
            nobounds,
        } => HIRStmt::Switch {
            id,
            span,
            expr: ast_expr_to_hir(expr),
            cases: cases.into_iter().map(ast_case_to_hir).collect(),
            nobounds,
        },
        Stmt::While {
            id,
            span,
            cond,
            body,
        } => HIRStmt::While {
            id,
            span,
            cond: ast_expr_to_hir(cond),
            body: Box::new(ast_stmt_to_hir(*body)),
        },
        Stmt::For {
            id,
            span,
            init,
            cond,
            step,
            body,
        } => HIRStmt::For {
            id,
            span,
            init: init.map(|s| Box::new(ast_stmt_to_hir(*s))),
            cond: cond.map(ast_expr_to_hir),
            step: step.map(ast_expr_to_hir),
            body: Box::new(ast_stmt_to_hir(*body)),
        },
        Stmt::Return { id, span, expr } => HIRStmt::Return {
            id,
            span,
            expr: expr.map(ast_expr_to_hir),
        },
        Stmt::Break { id, span } => HIRStmt::Break { id, span },
        Stmt::Continue { id, span } => HIRStmt::Continue { id, span },
    }
}

fn ast_case_to_hir(case: Case) -> HIRCase {
    HIRCase {
        values: case.values,
        body: ast_stmt_to_hir(case.body),
    }
}

fn ast_expr_to_hir(expr: Expr) -> HIRExpr {
    match expr {
        Expr::Ident { id, span, name } => HIRExpr::Ident {
            id,
            span,
            name,
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Constant { id, span, value } => HIRExpr::Constant {
            id,
            span,
            value,
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Assign { id, span, lhs, rhs } => HIRExpr::Assign {
            id,
            span,
            lhs: Box::new(ast_expr_to_hir(*lhs)),
            rhs: Box::new(ast_expr_to_hir(*rhs)),
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Binary {
            id,
            span,
            op,
            lhs,
            rhs,
        } => HIRExpr::Binary {
            id,
            span,
            op,
            lhs: Box::new(ast_expr_to_hir(*lhs)),
            rhs: Box::new(ast_expr_to_hir(*rhs)),
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Unary { id, span, op, expr } => HIRExpr::Unary {
            id,
            span,
            op,
            expr: Box::new(ast_expr_to_hir(*expr)),
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Call {
            id,
            span,
            func,
            args,
        } => HIRExpr::Call {
            id,
            span,
            func: Box::new(ast_expr_to_hir(*func)),
            args: args.into_iter().map(ast_expr_to_hir).collect(),
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Member {
            id,
            span,
            base,
            field,
            arrow,
        } => HIRExpr::Member {
            id,
            span,
            base: Box::new(ast_expr_to_hir(*base)),
            field,
            arrow,
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Index {
            id,
            span,
            base,
            index,
        } => HIRExpr::Index {
            id,
            span,
            base: Box::new(ast_expr_to_hir(*base)),
            index: Box::new(ast_expr_to_hir(*index)),
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Conditional {
            id,
            span,
            cond,
            then_expr,
            else_expr,
        } => HIRExpr::Conditional {
            id,
            span,
            cond: Box::new(ast_expr_to_hir(*cond)),
            then_expr: Box::new(ast_expr_to_hir(*then_expr)),
            else_expr: Box::new(ast_expr_to_hir(*else_expr)),
            ty: Type::Inbuilt(InbuiltType::U0),
        },
        Expr::Cast { id, span, ty, expr } => HIRExpr::Cast {
            id,
            span,
            ty: *ty,
            expr: Box::new(ast_expr_to_hir(*expr)),
        },
        Expr::Paren { id, span, expr } => HIRExpr::Paren {
            id,
            span,
            expr: Box::new(ast_expr_to_hir(*expr)),
            ty: Type::Inbuilt(InbuiltType::U0),
        },
    }
}

pub fn infer_types(modules: &mut [HIRModule]) {
    for module in modules {
        let mut globals = SymTable::new();
        for decl in &module.decls {
            if let HIRDeclaration::Var { name, ty, .. } = decl {
                globals.insert(name.clone(), SymEntry { ty: ty.clone() });
            }
        }
        for decl in &mut module.decls {
            infer_decl_types(decl, &mut globals);
        }
    }
}

fn infer_decl_types(decl: &mut HIRDeclaration, globals: &mut SymTable) {
    match decl {
        HIRDeclaration::Var { init, .. } => {
            if let Some(expr) = init {
                let mut table = globals.clone();
                infer_expr(expr, &mut table);
            }
        }
        HIRDeclaration::Func { params, body, .. } => {
            let mut locals: SymTable = params
                .iter()
                .map(|p| (p.name.clone(), SymEntry { ty: p.ty.clone() }))
                .collect();
            infer_stmt(body, &mut locals, globals);
        }
        HIRDeclaration::Struct { fields, .. } => {
            for f in fields {
                infer_decl_types(f, globals);
            }
        }
        HIRDeclaration::Enum { variants, .. } => {
            for v in variants {
                if let Some(expr) = &mut v.value {
                    infer_expr(expr, &mut globals.clone());
                }
            }
        }
        HIRDeclaration::Include { .. } => {}
    }
}

fn infer_stmt(stmt: &mut HIRStmt, locals: &mut SymTable, globals: &SymTable) {
    match stmt {
        HIRStmt::Expr { expr, ty, .. } => {
            infer_expr(expr, locals);
            *ty = expr.ty();
        }
        HIRStmt::Block { stmts, .. } => {
            for s in stmts {
                infer_stmt(s, locals, globals);
            }
        }
        HIRStmt::VarDecl { name, ty, init, .. } => {
            if let Some(e) = init {
                infer_expr(e, locals);
                // *ty = e.ty();
            }
            locals.insert(name.clone(), SymEntry { ty: ty.clone() });
        }
        HIRStmt::If {
            cond,
            then_branch,
            ladder,
            else_branch,
            ..
        } => {
            infer_expr(cond, locals);
            infer_stmt(then_branch, locals, globals);
            for (c, s) in ladder {
                infer_expr(c, locals);
                infer_stmt(s, locals, globals);
            }
            if let Some(e) = else_branch {
                infer_stmt(e, locals, globals);
            }
        }
        HIRStmt::Switch { expr, cases, .. } => {
            infer_expr(expr, locals);
            for case in cases {
                infer_stmt(&mut case.body, locals, globals);
            }
        }
        HIRStmt::While { cond, body, .. } => {
            infer_expr(cond, locals);
            infer_stmt(body, locals, globals);
        }
        HIRStmt::For {
            init,
            cond,
            step,
            body,
            ..
        } => {
            if let Some(i) = init {
                infer_stmt(i, locals, globals);
            }
            if let Some(c) = cond {
                infer_expr(c, locals);
            }
            if let Some(s) = step {
                infer_expr(s, locals);
            }
            infer_stmt(body, locals, globals);
        }
        HIRStmt::Return { expr, .. } => {
            if let Some(e) = expr {
                infer_expr(e, locals);
            }
        }
        HIRStmt::Break { .. } | HIRStmt::Continue { .. } => {}
    }
}

fn infer_expr(expr: &mut HIRExpr, locals: &mut SymTable) {
    match expr {
        HIRExpr::Ident { name, ty, .. } => {
            if let Some(entry) = locals.get(name) {
                *ty = entry.ty.clone();
            }
        }
        HIRExpr::Constant { value, ty, .. } => {
            let t = match value {
                Constant::Int(_) => Type::Inbuilt(InbuiltType::I64),
                Constant::UInt(_) => Type::Inbuilt(InbuiltType::U64),
                Constant::Float(_) => Type::Inbuilt(InbuiltType::F64),
                Constant::Bool(_) => Type::Inbuilt(InbuiltType::Bool),
                Constant::Char(_) => Type::Inbuilt(InbuiltType::U8),
                _ => Type::Inbuilt(InbuiltType::U0),
            };
            *ty = t;
        }
        HIRExpr::Assign { lhs, rhs, ty, .. } => {
            infer_expr(lhs, locals);
            infer_expr(rhs, locals);
            // rhs.ty_mut().clone_from(&lhs.ty());
            *ty = lhs.ty();
        }
        HIRExpr::Binary { lhs, rhs, ty, .. } => {
            infer_expr(lhs, locals);
            infer_expr(rhs, locals);
            *ty = unify_types(lhs.ty(), rhs.ty());
        }
        HIRExpr::Unary { expr: e, ty, .. } => {
            infer_expr(e, locals);
            *ty = e.ty();
        }
        HIRExpr::Call { func, args, ty, .. } => {
            infer_expr(func, locals);
            for a in args {
                infer_expr(a, locals);
            }
            *ty = func.ty();
        }
        HIRExpr::Member { base, ty, .. } => {
            infer_expr(base, locals);
            *ty = base.ty();
        }
        HIRExpr::Index {
            base, index, ty, ..
        } => {
            infer_expr(base, locals);
            infer_expr(index, locals);
            *ty = base.ty();
        }
        HIRExpr::Conditional {
            cond,
            then_expr,
            else_expr,
            ty,
            ..
        } => {
            infer_expr(cond, locals);
            infer_expr(then_expr, locals);
            infer_expr(else_expr, locals);
            *ty = unify_types(then_expr.ty(), else_expr.ty());
        }
        HIRExpr::Cast { expr: e, ty: t, .. } => {
            infer_expr(e, locals);
            *t = t.clone();
        }
        HIRExpr::Paren { expr: e, ty, .. } => {
            infer_expr(e, locals);
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

// this is so cool and big brain

pub trait HIRExprGives {
    fn ty(&self) -> Type;
    fn ty_mut(&mut self) -> &mut Type;
    fn id(&self) -> NodeId;
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

    fn id(&self) -> NodeId {
        match self {
            HIRExpr::Ident { id, .. }
            | HIRExpr::Constant { id, .. }
            | HIRExpr::Assign { id, .. }
            | HIRExpr::Binary { id, .. }
            | HIRExpr::Unary { id, .. }
            | HIRExpr::Call { id, .. }
            | HIRExpr::Member { id, .. }
            | HIRExpr::Index { id, .. }
            | HIRExpr::Conditional { id, .. }
            | HIRExpr::Cast { id, .. }
            | HIRExpr::Paren { id, .. } => *id,
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
