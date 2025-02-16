
pub struct ExprRoot
{
    pub e: Expr,
}

pub struct Block
{
    pub statements: Vec<Statement>,
    pub result: Option<Box<Expr>>,
}
pub enum Statement
{
    Expr(Expr),
    Let(super::Pattern, super::Type, Option<Expr>),
}

pub enum IntLitClass {
    Unspecified,
    Pointer,
    Integer(super::ty::IntClass),
}
pub enum Expr
{
    Block(Block),
    LiteralString(Vec<u8>),
    LiteralInteger(u128, IntLitClass),

    NamedValue(super::Path),
    CallPath(super::Path, Vec<Expr>),

    FieldNamed(Box<Expr>, crate::Ident),
    FieldIndex(Box<Expr>, u128),
    Index(Box<Expr>, Box<Expr>),

    Deref(Box<Expr>),
    Cast(Box<Expr>, super::Type),

    UniOp(UniOpTy, Box<Expr>),
    BinOp(BinOpTy, Box<Expr>, Box<Expr>),
    CallValue(Box<Expr>, Vec<Expr>),

    ForLoop {
        pattern: super::Pattern,
        start: Box<Expr>,
        end: Box<Expr>,
        body: Block,
        else_block: Option<Block>,
    }
}

pub enum BinOpTy
{
    Add,
    Sub,
    Mul,
    Div,
    Rem,

    BitAnd,
    BitOr,
    BitXor,
}

pub enum UniOpTy
{
    Invert,
    Negate,
}

pub enum ExpressionTy
{
    Literal,
}