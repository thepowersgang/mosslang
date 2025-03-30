
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
    pub span: crate::Span,
    pub kind: ExprKind,
    pub data_ty: super::Type,
}
impl ExprKind {
    pub fn to_expr(self, span: crate::Span) -> Expr {
        Expr { span, kind: self, data_ty: crate::ast::Type::new_infer() }
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
        op: Option<AssignOp>,
        value: Box<Expr>,
    },

    NamedValue(super::Path, Option<super::path::ValueBinding>),
    CallPath(super::Path, Option<super::path::ValueBinding>, Vec<Expr>),
    Tuple(Vec<Expr>),
    //Struct(super::Path, Vec<Expr>),

    FieldNamed(Box<Expr>, crate::Ident),
    FieldIndex(Box<Expr>, usize),
    Index(Box<Expr>, Box<Expr>),

    /// is_mut
    Addr(bool, Box<Expr>),
    Deref(Box<Expr>),
    /// Type cast operation
    Cast(Box<Expr>, super::Type),
    /// Coercion point, injected by type checking
    Coerce(Box<Expr>),

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
            write!(f, "NamedValue({:?} [{:?}])", path, value_binding),
        ExprKind::CallPath(path, value_binding, exprs) => {
            write!(f, "CallPath({:?} [{:?}] ( ", path, value_binding)?;
            for _ in exprs { 
                f.write_str("..., ")?;
            }
            f.write_str(")")
            },
        ExprKind::CallValue(_val, exprs) => {
            write!(f, "CallValue( (...)( ")?;
            for _ in exprs { 
                f.write_str("..., ")?;
            }
            f.write_str(") )")
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
        ExprKind::Addr(is_mut, _expr) => 
            write!(f, "Addr({}, ...)", if *is_mut { "mut" } else { "const" }),
        ExprKind::Deref(_) =>
            write!(f, "Deref(...)"),
        ExprKind::Cast(_expr, ty) => 
            write!(f, "Cast(... as {:?})", ty),
        ExprKind::Coerce(_expr) => 
            write!(f, "Coerce(...)"),
        ExprKind::UniOp(uni_op_ty, _expr) =>
            write!(f, "UniOp({:?}, ...)", uni_op_ty),
        ExprKind::BinOp(bin_op_ty, _expr_l, _expr_r) =>
            write!(f, "BinOp(... {:?} ...)", bin_op_ty),
        ExprKind::Loop { body: _ } => todo!(),
        ExprKind::WhileLoop { cond: _, body: _, else_block } => 
            write!(f, "WhileLoop( ... else {})", if else_block.is_none() { "() " } else { "..." }),
        ExprKind::ForLoop { pattern, start: _, end: _, body: _, else_block } =>
            write!(f, "ForLoop({:?} in <a> .. <b> ... else {})", pattern, if else_block.is_none() { "() " } else { "..." }),
        ExprKind::IfChain { branches, else_block } =>
            write!(f, "IfChain(x{} else {})", branches.len(), if else_block.is_none() { "() " } else { "..." }),
        ExprKind::Match { value: _, branches } =>
            write!(f, "Match({} branches)", branches.len()),
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
pub enum AssignOp
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

#[derive(Debug, Clone, Copy)]
pub enum UniOpTy
{
    Invert,
    Negate,
}
