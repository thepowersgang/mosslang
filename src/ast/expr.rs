
#[derive(Debug)]
pub struct ExprRoot
{
    pub e: Expr,
    /// Number of variables needed/allocated, populated in resolve
    pub variable_count: usize,
    /// Variable types, populated after typecheck
    pub variables: Vec<super::Type>,
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

#[derive(Debug)]
pub enum IntLitClass {
    Unspecified,
    Pointer,
    Integer(super::ty::IntClass),
}
#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub data_ty: super::Type,
}
impl From<ExprKind> for Expr {
    fn from(value: ExprKind) -> Self {
        Expr { kind: value, data_ty: crate::ast::Type::new_infer() }
    }
}
pub enum ExprKind
{
    Block(Block),
    LiteralString(crate::ast::StringLiteral),
    LiteralInteger(u128, IntLitClass),

    Return(Option<Box<Expr>>),
    Continue,
    Break(Option<Box<Expr>>),

    Assign {
        slot: Box<Expr>,
        op: Option<()>,
        value: Box<Expr>,
    },

    NamedValue(super::Path, Option<super::path::ValueBinding>),
    CallPath(super::Path, Option<super::path::ValueBinding>, Vec<Expr>),
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
impl<'a> ::core::fmt::Debug for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use crate::ast::expr::ExprKind;
        match self {
        ExprKind::Block(_) =>
            write!(f, "Block(...)"),
        ExprKind::LiteralString(s) =>
            write!(f, "LiteralString({:?})", s),
        ExprKind::LiteralInteger(v, int_lit_class) =>
            write!(f, "LiteralInteger({}, {:?})", v, int_lit_class),
        ExprKind::Return(expr) =>
            write!(f, "Return({})", if expr.is_none() { "void" } else { "..." }),
        ExprKind::Continue =>
            write!(f, "Continue"),
        ExprKind::Break(expr) =>
            write!(f, "Break({})", if expr.is_none() { "void" } else { "..." }),
        ExprKind::Assign { slot: _, op, value: _ } =>
            write!(f, "Assign(..., {:?}, ...)", op),
        ExprKind::NamedValue(path, value_binding) =>
            write!(f, "NamedValue({:?} [{:?})", path, value_binding),
        ExprKind::CallPath(path, value_binding, exprs) => {
            write!(f, "CallPath({:?} [{:?}] ( ", path, value_binding)?;
            for _ in exprs { 
                f.write_str("..., ")?;
            }
            f.write_str(")")
            }
        ExprKind::Tuple(exprs) => {
            f.write_str("( ")?;
            for _ in exprs { 
                f.write_str("..., ")?;
            }
            f.write_str(")")
        },
        ExprKind::FieldNamed(_expr, ident) =>
            write!(f, "(...).{}", ident),
        ExprKind::FieldIndex(_value, _idx) => todo!(),
        ExprKind::Index(_expr_v, _expr_i) =>
            write!(f, "Index(..., ...)"),
        ExprKind::Addr(_, expr) => todo!(),
        ExprKind::Deref(_) =>
            write!(f, "Deref(...)"),
        ExprKind::Cast(expr, _) => todo!(),
        ExprKind::UniOp(uni_op_ty, _expr) =>
            write!(f, "UniOp({:?}, ...)", uni_op_ty),
        ExprKind::BinOp(bin_op_ty, _expr_l, _expr_r) =>
            write!(f, "BinOp(... {:?} ...)", bin_op_ty),
        ExprKind::CallValue(expr, exprs) => todo!(),
        ExprKind::Loop { body } => todo!(),
        ExprKind::WhileLoop { cond, body, else_block } => todo!(),
        ExprKind::ForLoop { pattern, start: _, end: _, body: _, else_block } =>
            write!(f, "ForLoop({:?} in <a> .. <b> ... else {})", pattern, if else_block.is_none() { "() " } else { "..." }),
        ExprKind::IfChain { branches, else_block } =>
            write!(f, "IfChain(x{} else {})", branches.len(), if else_block.is_none() { "() " } else { "..." }),
        ExprKind::Match { value, branches } => todo!(),
        }
    }
}
pub struct IfCondition {
    pub cond: Expr,
    pub body: Block,
}
pub struct MatchArm {
    pub pat: super::Pattern,
    pub val: Expr,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
pub enum UniOpTy
{
    Invert,
    Negate,
}
