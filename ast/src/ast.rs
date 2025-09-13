use ecow::EcoString;

use crate::span::Span;

pub type NodeId = u32;
pub type Label = EcoString;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub decls: Vec<Declaration>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Var {
        span: Span,
        name: Label,
        ty: Type,
        init: Option<Expr>,
    },
    Func {
        span: Span,
        name: Label,
        ret_ty: Type,
        params: Vec<Param>,
        body: Stmt,
    },
    Class {
        span: Span,
        name: Label,
        fields: Vec<(Type, Label)>,
    },
    Enum {
        span: Span,
        name: Option<Label>,
        variants: Vec<Enumerator>,
    },
    Include {
        name: Label,
        as_name: Option<EcoString>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub span: Span,
    pub name: Label,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Enumerator {
    pub span: Span,
    pub name: Label,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Inbuilt(InbuiltType),
    Named(Label),
    Pointer(Box<Type>),
    Array(Box<Type>, Option<Expr>),
    Function {
        params: Vec<Type>,
        ret: Box<Type>,
        varargs: bool,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum InbuiltType {
    U0,
    U8,
    U16,
    U32,
    U64,
    I8,
    I16,
    I32,
    I64,
    F64,
    Bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Expr {
        span: Span,
        expr: Expr,
    },
    Block {
        span: Span,
        stmts: Vec<Stmt>,
    },
    VarDecl {
        span: Span,
        name: Label,
        ty: Type,
        init: Option<Expr>,
    },
    If {
        span: Span,
        cond_then_ladder: Vec<(Expr, Stmt)>,
        else_branch: Option<Box<Stmt>>,
    },
    Switch {
        span: Span,
        subject: Expr,
        cases: Vec<(Constant, Vec<Stmt>)>,
        default: Option<Vec<Stmt>>,
        // switch [i] -> can skip default
        // read https://holyc-lang.com/docs/language-spec/learn-control-flow#without-bounds-checking
        nobounds: bool,
    },

    While {
        span: Span,
        cond: Expr,
        body: Box<Stmt>,
    },
    For {
        span: Span,
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Box<Stmt>,
    },
    Return {
        span: Span,
        expr: Option<Expr>,
    },
    Break {
        span: Span,
    },
    Continue {
        span: Span,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Case {
    pub values: Constant,
    pub body: Stmt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident {
        span: Span,
        name: Label,
    },
    Constant {
        span: Span,
        value: Constant,
    },
    Assign {
        span: Span,
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Binary {
        span: Span,
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        span: Span,
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        span: Span,
        func: Label,
        args: Vec<Expr>,
    },
    Member {
        span: Span,
        base: Box<Expr>,
        field: Label,
        arrow: bool,
    },
    Index {
        span: Span,
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Conditional {
        span: Span,
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Cast {
        span: Span,
        ty: Box<Type>,
        expr: Box<Expr>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Constant {
    Int(i64),
    UInt(u64),
    Float(f64),
    Char(char),
    String(String),
    Bool(bool),
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Shl,
    Shr,
    Lt,
    Le,
    Gt,
    Ge,
    EqEq,
    Ne,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,

    /* these are assignment ops but appear in the same context as a binary op */
    Eq,
    AddEq,
    SubEq,
    MulEq,
    DivEq,
    ModEq,
    BitAndEq,
    BitXorEq,
    BitOrEq,
    ShlEq,
    ShrEq,
}

impl BinaryOp {
    pub fn is_assignment_op(&self) -> bool {
        matches!(
            self,
            BinaryOp::Eq
                | BinaryOp::AddEq
                | BinaryOp::SubEq
                | BinaryOp::MulEq
                | BinaryOp::DivEq
                | BinaryOp::ModEq
                | BinaryOp::BitAndEq
                | BinaryOp::BitXorEq
                | BinaryOp::BitOrEq
                | BinaryOp::ShlEq
                | BinaryOp::ShrEq
        )
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    AddrOf,
    Deref,
    Plus,
    Minus,
    BitNot,
    LogNot,
}

// helpers

impl Declaration {
    pub fn var(span: Span, name: EcoString, ty: Type, init: Option<Expr>) -> Self {
        Self::Var {
            span,
            name,
            ty,
            init,
        }
    }

    pub fn func(span: Span, name: EcoString, ret_ty: Type, params: Vec<Param>, body: Stmt) -> Self {
        Self::Func {
            span,
            name,
            ret_ty,
            params,
            body,
        }
    }

    pub fn klass(span: Span, name: EcoString, fields: Vec<(Type, Label)>) -> Self {
        Self::Class { span, name, fields }
    }

    pub fn enumm(span: Span, name: Option<EcoString>, variants: Vec<Enumerator>) -> Self {
        Self::Enum {
            span,
            name,
            variants,
        }
    }
}

impl Type {
    pub fn inbuilt(t: InbuiltType) -> Self {
        Self::Inbuilt(t)
    }
    pub fn named(name: EcoString) -> Self {
        Self::Named(name)
    }
    pub fn pointer(inner: Type) -> Self {
        Self::Pointer(Box::new(inner))
    }
    pub fn array(inner: Type, size: Option<Expr>) -> Self {
        Self::Array(Box::new(inner), size)
    }
    pub fn function(params: Vec<Type>, ret: Type, varargs: bool) -> Self {
        Self::Function {
            params,
            ret: Box::new(ret),
            varargs,
        }
    }
}

impl Stmt {
    pub fn expr(span: Span, expr: Expr) -> Self {
        Self::Expr { span, expr }
    }
    pub fn block(span: Span, stmts: Vec<Stmt>) -> Self {
        Self::Block { span, stmts }
    }
    pub fn if_stmt(
        span: Span,
        cond_then_ladder: Vec<(Expr, Stmt)>,
        else_branch: Option<Stmt>,
    ) -> Self {
        Self::If {
            span,
            cond_then_ladder,
            else_branch: else_branch.map(Box::new),
        }
    }
    pub fn while_stmt(span: Span, cond: Expr, body: Stmt) -> Self {
        Self::While {
            span,
            cond,
            body: Box::new(body),
        }
    }
    pub fn for_stmt(
        span: Span,
        init: Option<Stmt>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Stmt,
    ) -> Self {
        Self::For {
            span,
            init: init.map(Box::new),
            cond,
            step,
            body: Box::new(body),
        }
    }
    pub fn return_stmt(span: Span, expr: Option<Expr>) -> Self {
        Self::Return { span, expr }
    }
    pub fn break_stmt(span: Span) -> Self {
        Self::Break { span }
    }
    pub fn continue_stmt(span: Span) -> Self {
        Self::Continue { span }
    }
}

impl Expr {
    pub fn ident(span: Span, name: EcoString) -> Self {
        Self::Ident { span, name }
    }
    pub fn constant(span: Span, value: Constant) -> Self {
        Self::Constant { span, value }
    }
    pub fn assign(span: Span, op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        Self::Assign {
            span,
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn binary(span: Span, op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        Self::Binary {
            span,
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn unary(span: Span, op: UnaryOp, expr: Expr) -> Self {
        Self::Unary {
            span,
            op,
            expr: Box::new(expr),
        }
    }
    pub fn call(span: Span, func: Label, args: Vec<Expr>) -> Self {
        Self::Call { span, func, args }
    }
    pub fn member(span: Span, base: Expr, field: EcoString, arrow: bool) -> Self {
        Self::Member {
            span,
            base: Box::new(base),
            field,
            arrow,
        }
    }
    pub fn index(span: Span, base: Expr, index: Expr) -> Self {
        Self::Index {
            span,
            base: Box::new(base),
            index: Box::new(index),
        }
    }
    pub fn conditional(span: Span, cond: Expr, then_expr: Expr, else_expr: Expr) -> Self {
        Self::Conditional {
            span,
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        }
    }
    pub fn cast(span: Span, ty: Type, expr: Expr) -> Self {
        Self::Cast {
            span,
            ty: Box::new(ty),
            expr: Box::new(expr),
        }
    }
}
