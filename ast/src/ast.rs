use crate::span::Span;

pub type NodeId = u32;
pub type Identifier = String;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub decls: Vec<DeclarationOrStmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DeclarationOrStmt {
    Declaration(Declaration),
    Stmt(Stmt),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Declaration {
    Var {
        id: NodeId,
        span: Span,
        name: Identifier,
        ty: Type,
        init: Option<Expr>,
    },
    Func {
        id: NodeId,
        span: Span,
        name: Identifier,
        ret_ty: Type,
        params: Vec<Param>,
        body: Stmt,
    },
    Struct {
        id: NodeId,
        span: Span,
        name: Option<Identifier>,
        fields: Vec<Declaration>, // dumbed down
    },
    Enum {
        id: NodeId,
        span: Span,
        name: Option<Identifier>,
        variants: Vec<Enumerator>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Param {
    pub id: NodeId,
    pub span: Span,
    pub name: Identifier,
    pub ty: Type,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Enumerator {
    pub id: NodeId,
    pub span: Span,
    pub name: Identifier,
    pub value: Option<Expr>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Inbuilt(InbuiltType),
    Named(Identifier),
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
        id: NodeId,
        span: Span,
        expr: Expr,
    },
    Block {
        id: NodeId,
        span: Span,
        stmts: Vec<Stmt>,
    },
    If {
        id: NodeId,
        span: Span,
        cond: Expr,
        then_branch: Box<Stmt>,
        ladder: Vec<(Expr, Stmt)>,
        else_branch: Option<Box<Stmt>>,
    },
    Switch {
        id: NodeId,
        span: Span,
        expr: Expr,
        cases: Vec<Case>,
        // switch [i] -> can skip default
        // read https://holyc-lang.com/docs/language-spec/learn-control-flow#without-bounds-checking
        nobounds: bool,
    },

    While {
        id: NodeId,
        span: Span,
        cond: Expr,
        body: Box<Stmt>,
    },
    For {
        id: NodeId,
        span: Span,
        init: Option<Box<Stmt>>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Box<Stmt>,
    },
    Return {
        id: NodeId,
        span: Span,
        expr: Option<Expr>,
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
pub struct Case {
    // possible TODO: Constant -> Expr???
    pub values: Option<Vec<Constant>>,
    pub body: Stmt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Ident {
        id: NodeId,
        span: Span,
        name: Identifier,
    },
    Constant {
        id: NodeId,
        span: Span,
        value: Constant,
    },
    Assign {
        id: NodeId,
        span: Span,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Binary {
        id: NodeId,
        span: Span,
        op: BinaryOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Unary {
        id: NodeId,
        span: Span,
        op: UnaryOp,
        expr: Box<Expr>,
    },
    Call {
        id: NodeId,
        span: Span,
        func: Box<Expr>,
        args: Vec<Expr>,
    },
    Member {
        id: NodeId,
        span: Span,
        base: Box<Expr>,
        field: Identifier,
        arrow: bool,
    },
    Index {
        id: NodeId,
        span: Span,
        base: Box<Expr>,
        index: Box<Expr>,
    },
    Conditional {
        id: NodeId,
        span: Span,
        cond: Box<Expr>,
        then_expr: Box<Expr>,
        else_expr: Box<Expr>,
    },
    Cast {
        id: NodeId,
        span: Span,
        ty: Box<Type>,
        expr: Box<Expr>,
    },
    Paren {
        id: NodeId,
        span: Span,
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

#[derive(Clone, Debug, PartialEq)]
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
    Eq,
    Ne,
    BitAnd,
    BitXor,
    BitOr,
    And,
    Or,
}

#[derive(Clone, Debug, PartialEq)]
pub enum UnaryOp {
    PreInc,
    PreDec,
    AddrOf,
    Deref,
    Plus,
    Minus,
    BitNot,
    LogNot,
}

// helpers

impl Declaration {
    pub fn var(id: NodeId, span: Span, name: String, ty: Type, init: Option<Expr>) -> Self {
        Self::Var {
            id,
            span,
            name,
            ty,
            init,
        }
    }

    pub fn func(
        id: NodeId,
        span: Span,
        name: String,
        ret_ty: Type,
        params: Vec<Param>,
        body: Stmt,
    ) -> Self {
        Self::Func {
            id,
            span,
            name,
            ret_ty,
            params,
            body,
        }
    }

    pub fn strukt(id: NodeId, span: Span, name: Option<String>, fields: Vec<Declaration>) -> Self {
        Self::Struct {
            id,
            span,
            name,
            fields,
        }
    }

    pub fn enumm(id: NodeId, span: Span, name: Option<String>, variants: Vec<Enumerator>) -> Self {
        Self::Enum {
            id,
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
    pub fn named(name: String) -> Self {
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
    pub fn expr(id: NodeId, span: Span, expr: Expr) -> Self {
        Self::Expr { id, span, expr }
    }
    pub fn block(id: NodeId, span: Span, stmts: Vec<Stmt>) -> Self {
        Self::Block { id, span, stmts }
    }
    pub fn if_stmt(
        id: NodeId,
        span: Span,
        cond: Expr,
        then_branch: Stmt,
        else_if: Vec<(Expr, Stmt)>,
        else_branch: Option<Stmt>,
    ) -> Self {
        Self::If {
            id,
            span,
            cond,
            then_branch: Box::new(then_branch),
            ladder: else_if, // keep old name if you prefer
            else_branch: else_branch.map(Box::new),
        }
    }
    pub fn while_stmt(id: NodeId, span: Span, cond: Expr, body: Stmt) -> Self {
        Self::While {
            id,
            span,
            cond,
            body: Box::new(body),
        }
    }
    pub fn for_stmt(
        id: NodeId,
        span: Span,
        init: Option<Stmt>,
        cond: Option<Expr>,
        step: Option<Expr>,
        body: Stmt,
    ) -> Self {
        Self::For {
            id,
            span,
            init: init.map(Box::new),
            cond,
            step,
            body: Box::new(body),
        }
    }
    pub fn return_stmt(id: NodeId, span: Span, expr: Option<Expr>) -> Self {
        Self::Return { id, span, expr }
    }
    pub fn break_stmt(id: NodeId, span: Span) -> Self {
        Self::Break { id, span }
    }
    pub fn continue_stmt(id: NodeId, span: Span) -> Self {
        Self::Continue { id, span }
    }
}

impl Expr {
    pub fn ident(id: NodeId, span: Span, name: String) -> Self {
        Self::Ident { id, span, name }
    }
    pub fn constant(id: NodeId, span: Span, value: Constant) -> Self {
        Self::Constant { id, span, value }
    }
    pub fn assign(id: NodeId, span: Span, lhs: Expr, rhs: Expr) -> Self {
        Self::Assign {
            id,
            span,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn binary(id: NodeId, span: Span, op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        Self::Binary {
            id,
            span,
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        }
    }
    pub fn unary(id: NodeId, span: Span, op: UnaryOp, expr: Expr) -> Self {
        Self::Unary {
            id,
            span,
            op,
            expr: Box::new(expr),
        }
    }
    pub fn call(id: NodeId, span: Span, func: Expr, args: Vec<Expr>) -> Self {
        Self::Call {
            id,
            span,
            func: Box::new(func),
            args,
        }
    }
    pub fn member(id: NodeId, span: Span, base: Expr, field: String, arrow: bool) -> Self {
        Self::Member {
            id,
            span,
            base: Box::new(base),
            field,
            arrow,
        }
    }
    pub fn index(id: NodeId, span: Span, base: Expr, index: Expr) -> Self {
        Self::Index {
            id,
            span,
            base: Box::new(base),
            index: Box::new(index),
        }
    }
    pub fn conditional(
        id: NodeId,
        span: Span,
        cond: Expr,
        then_expr: Expr,
        else_expr: Expr,
    ) -> Self {
        Self::Conditional {
            id,
            span,
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
        }
    }
    pub fn cast(id: NodeId, span: Span, ty: Type, expr: Expr) -> Self {
        Self::Cast {
            id,
            span,
            ty: Box::new(ty),
            expr: Box::new(expr),
        }
    }
    pub fn paren(id: NodeId, span: Span, expr: Expr) -> Self {
        Self::Paren {
            id,
            span,
            expr: Box::new(expr),
        }
    }
}
