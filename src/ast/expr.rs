
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
pub struct Expr {
    pub kind: ExprKind,
    // TODO: Type for typecheck
}
impl From<ExprKind> for Expr {
    fn from(value: ExprKind) -> Self {
        Expr { kind: value }
    }
}
pub enum ExprKind
{
    Block(Block),
    LiteralString(Vec<u8>),
    LiteralInteger(u128, IntLitClass),

    Return(Option<Box<Expr>>),
    Continue(Option<Box<Expr>>),
    Break(Option<Box<Expr>>),

    Assign {
        slot: Box<Expr>,
        op: Option<()>,
        value: Box<Expr>,
    },

    NamedValue(super::Path),
    CallPath(super::Path, Vec<Expr>),
    Tuple(Vec<Expr>),
    //Struct(super::Path, Vec<Expr>),

    FieldNamed(Box<Expr>, crate::Ident),
    FieldIndex(Box<Expr>, u128),
    Index(Box<Expr>, Box<Expr>),

    Addr(bool, Box<Expr>),
    Deref(Box<Expr>),
    Cast(Box<Expr>, super::Type),

    UniOp(UniOpTy, Box<Expr>),
    BinOp(BinOpTy, Box<Expr>, Box<Expr>),
    CallValue(Box<Expr>, Vec<Expr>),

    Loop {
        body: Block,
    },
    WhileLoop {
        cond: Box<Expr>,
        body: Block,
        else_block: Option<Block>,
    },
    ForLoop {
        pattern: super::Pattern,
        start: Box<Expr>,
        end: Box<Expr>,
        body: Block,
        else_block: Option<Block>,
    },
    IfChain {
        branches: Vec<IfCondition>,
        else_block: Option<Block>,
    },
    Match {
        value: Box<Expr>,
        branches: Vec<MatchArm>,
    },
}
pub struct IfCondition {
    pub cond: Expr,
    pub body: Block,
}
pub struct MatchArm {
    pub pat: super::Pattern,
    pub val: Expr,
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

    Shl,
    Shr,

    Equals, NotEquals,
    Lt, LtEquals,
    Gt, GtEquals,

    BoolAnd,
    BoolOr,
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