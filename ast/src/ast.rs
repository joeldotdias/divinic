use crate::span::Span;

pub type NodeId = u32;
pub type Identifier = String;

#[derive(Clone, Debug, PartialEq)]
pub struct Module {
    pub id: NodeId,
    pub span: Span,
    pub items: Vec<Decl>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Function {
        id: NodeId,
        span: Span,
        specifiers: Vec<Specifier>,
        declarator: Declarator,
        body: Statement,
    },
    Variable {
        id: NodeId,
        span: Span,
        specifiers: Vec<Specifier>,
        init: Option<Expression>,
    },
    ClassOrUnion {
        id: NodeId,
        span: Span,
        kind: ClassOrUnionKind,
        name: Option<Identifier>,
        members: Vec<ClassDecl>,
    },
    Enum {
        id: NodeId,
        span: Span,
        name: Option<Identifier>,
        enumerators: Vec<Enumerator>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Specifier {
    Storage {
        id: NodeId,
        span: Span,
        class: StorageClassSpecifier,
    },
    Type {
        id: NodeId,
        span: Span,
        kind: TypeSpecifier,
    },
    Qualifier {
        id: NodeId,
        span: Span,
        qual: TypeQualifier,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum StorageClassSpecifier {
    Reg,
    NoReg,
    Public,
    Static,
    Extern,
    UnderscoreExtern,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpecifier {
    U0,
    U8,
    U16,
    U32,
    U64,
    I0,
    I8,
    I16,
    I32,
    I64,
    F64,
    ClassOrUnion(ClassOrUnionKind, Identifier),
    Enum(Identifier),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeQualifier {
    Const,
    Volatile,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarator {
    pub id: NodeId,
    pub span: Span,
    pub direct: Box<DirectDeclarator>,
    pub pointer: Option<Box<Declarator>>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectDeclarator {
    Ident {
        id: NodeId,
        span: Span,
        name: Identifier,
    },
    Paren {
        id: NodeId,
        span: Span,
        inner: Box<Declarator>,
    },
    Array {
        id: NodeId,
        span: Span,
        base: Box<Declarator>,
        size: Option<Expression>,
    },
    Function {
        id: NodeId,
        span: Span,
        base: Box<Declarator>,
        params: Vec<Decl>,
        varargs: bool,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    ExprStmt {
        id: NodeId,
        span: Span,
        expr: Expression,
    },
    Compound {
        id: NodeId,
        span: Span,
        items: Vec<Statement>,
    },
    If {
        id: NodeId,
        span: Span,
        cond: Expression,
        then_branch: Box<Statement>,
        else_branch: Option<Box<Statement>>,
    },
    While {
        id: NodeId,
        span: Span,
        cond: Expression,
        body: Box<Statement>,
    },
    DoWhile {
        id: NodeId,
        span: Span,
        body: Box<Statement>,
        cond: Expression,
    },
    For {
        id: NodeId,
        span: Span,
        init: Option<Box<Statement>>,
        cond: Option<Expression>,
        step: Option<Expression>,
        body: Box<Statement>,
    },
    Switch {
        id: NodeId,
        span: Span,
        cond: Expression,
        cases: Vec<Case>,
    },
    Return {
        id: NodeId,
        span: Span,
        expr: Option<Expression>,
    },
    Break {
        id: NodeId,
        span: Span,
    },
    Continue {
        id: NodeId,
        span: Span,
    },
    Goto {
        id: NodeId,
        span: Span,
        label: Identifier,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Case {
    pub id: NodeId,
    pub span: Span,
    pub value: Expression,
    pub body: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassOrUnionKind {
    Class,
    Union,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassDecl {
    pub id: NodeId,
    pub span: Span,
    pub specifiers: Vec<Specifier>,
    pub declarators: Vec<ClassDeclarator>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ClassDeclarator {
    Declarator {
        id: NodeId,
        span: Span,
        declarator: Declarator,
    },
    DeclaratorWithBitfield {
        id: NodeId,
        span: Span,
        declarator: Declarator,
        width: Expression,
    },
    Bitfield {
        id: NodeId,
        span: Span,
        width: Expression,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Enumerator {
    pub id: NodeId,
    pub span: Span,
    pub name: Identifier,
    pub value: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeName {
    pub id: NodeId,
    pub span: Span,
    pub specifiers: Vec<Specifier>,
    pub declarator: Option<Declarator>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
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
        lhs: Box<Expression>,
        op: AssignOp,
        rhs: Box<Expression>,
    },
    Binary {
        id: NodeId,
        span: Span,
        op: BinaryOp,
        lhs: Box<Expression>,
        rhs: Box<Expression>,
    },
    Unary {
        id: NodeId,
        span: Span,
        op: UnaryOp,
        expr: Box<Expression>,
    },
    Conditional {
        id: NodeId,
        span: Span,
        cond: Box<Expression>,
        then_expr: Box<Expression>,
        else_expr: Box<Expression>,
    },
    Call {
        id: NodeId,
        span: Span,
        func: Box<Expression>,
        args: Vec<Expression>,
    },
    Member {
        id: NodeId,
        span: Span,
        base: Box<Expression>,
        field: Identifier,
        arrow: bool,
    },
    Index {
        id: NodeId,
        span: Span,
        base: Box<Expression>,
        index: Box<Expression>,
    },
    Postfix {
        id: NodeId,
        span: Span,
        op: PostfixOp,
        expr: Box<Expression>,
    },
    Cast {
        id: NodeId,
        span: Span,
        ty: TypeName,
        expr: Box<Expression>,
    },
    SizeofExpr {
        id: NodeId,
        span: Span,
        expr: Box<Expression>,
    },
    SizeofType {
        id: NodeId,
        span: Span,
        ty: TypeName,
    },
    Comma {
        id: NodeId,
        span: Span,
        exprs: Vec<Expression>,
    },
    Paren {
        id: NodeId,
        span: Span,
        expr: Box<Expression>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub enum AssignOp {
    Assign,
    MulAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    ShlAssign,
    ShrAssign,
    AndAssign,
    XorAssign,
    OrAssign,
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

#[derive(Clone, Debug, PartialEq)]
pub enum PostfixOp {
    PostInc,
    PostDec,
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

