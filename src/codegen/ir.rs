//! A high-level cfg-based IR
//! 
//! NOT SSA!
//! 
//! This is similar to mrustc's MIR but it doesn't have the complex LValue setup, using pointers instead.
//!
//! 
//! Structure:
//! - Lowered expressions are a directed graph of "basic blocks"
//! - Blocks end with a "terminator" that jumps to another block
//! - Blocks contain "operations", that calculate or manipulate data
use crate::ast::path::AbsolutePath;

/// A read-only value (generated, or a read from a slot)
#[derive(Debug,Clone)]
pub enum Value {
    /// Indicates that this value can never be constructed
    Unreachable,
    /// A unit value from anything other than a literal `()`
    ImplicitUnit,
    /// Value stored in a local variable
    Local(LocalIndex, WrapperList),
    /// Value in a gloabl variable
    Named(AbsolutePath, WrapperList),
    /// Dereference a pointer and then apply wrappers
    Deref { ptr: LocalIndex, wrappers: WrapperList },
    /// String literal
    StringLiteral(crate::ast::StringLiteral),
    /// An integer literal
    IntegerLiteral(u128, crate::ast::ty::IntClass),
    /// A pointer to a function (could be a real function, or it could be a struct constructor, or an enum variant constructor)
    FunctionPointer(AbsolutePath, FunctionPointerTy),
}
impl Value {
    fn field(&self, idx: usize) -> Value {
        match self {
        Value::Unreachable => Value::Unreachable,
        Value::ImplicitUnit => panic!("Field on void"),
        Value::StringLiteral(..) => panic!("Field on string"),
        Value::IntegerLiteral(..) => panic!("Field on integer"),
        Value::FunctionPointer(..) => panic!("Field on function pointer"),
        Value::Local(root, wrappers) => Value::Local(*root, wrappers.with_added(Wrapper::Field(idx))),
        Value::Named(root, wrappers) => Value::Named(root.clone(), wrappers.with_added(Wrapper::Field(idx))),
        Value::Deref { ptr, wrappers } => Value::Deref { ptr: *ptr , wrappers: wrappers.with_added(Wrapper::Field(idx)) }
        }
    }
}
#[derive(Debug,Clone)]
pub enum FunctionPointerTy {
    Function,
    Struct,
    DataEnum(usize),
}

/// List of access wrappers to a value (indexing and field accesses)
/// 
/// Dereferencing is handled with different value types
#[derive(Default,Clone)]
pub(super) struct WrapperList(Vec<u32>);
impl WrapperList {
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn with_added(&self, w: Wrapper) -> Self {
        WrapperList(self.0.iter().copied()
            .chain(::std::iter::once(Self::encode(w)))
            .collect())
    }
    //pub fn add(mut self, w: Wrapper) -> Self {
    //    self.push(w);
    //    self
    //}
    pub fn push(&mut self, w: Wrapper) {
        self.0.push(Self::encode(w));
    }

    pub fn iter(&self) -> impl Iterator<Item=Wrapper> + '_ {
        self.0.iter().map(|&v|Self::decode(v))
    }

    fn encode(w: Wrapper) -> u32 {
        match w {
        Wrapper::Field(idx) => idx as u32,
        Wrapper::IndexBySlot(LocalIndex(s)) => (1 << 31) | s as u32,
        }
    }

    fn decode(v: u32) -> Wrapper {
        if v & 1 << 31 == 0 {
            Wrapper::Field(v as usize)
        }
        else {
            Wrapper::IndexBySlot(LocalIndex( (v & 0x7FFF_FFFF) as usize ))
        }
    }
}
impl ::core::fmt::Debug for WrapperList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("[")?;
        for v in &self.0 {
            write!(f, "{:?},", Self::decode(*v))?;
        }
        f.write_str("]")?;
        Ok( () )
    }
}
#[derive(Debug)]
pub(super) enum Wrapper {
    Field(usize),
    IndexBySlot(LocalIndex),
}
/// Non-branching operations (aka statements)
/// 
/// Most of these are just assignments
#[derive(Debug)]
pub enum Operation {
    Alloca {
        dst: LocalIndex,
        ty: crate::ast::Type,
    },
    /// Assign a value to a local variable
    AssignLocal(LocalIndex, Value),
    /// Assign a value to a dereference of a pointer (in a local variable)
    AssignDeref(LocalIndex, Value),

    /// Create an instance of a composite value (tuple or `struct`)
    CreateComposite(LocalIndex, Option<AbsolutePath>, Vec<Value>),

    /// Create an instance of a tagged-union enum
    CreateDataVariant(LocalIndex, AbsolutePath, usize, Vec<Value>),

    /// Cast a value to a different type (primitives only)
    ///
    /// - Type is determined by the slot type of the target.
    /// - Casts are possibly destructive (integer downcasts truncate, signed may change sign, floats discard)
    ///   - Signed->unsigned re-interprets (so `-1` becomes all one bits)
    ///   - Integer->float casts may saturate
    ///   - Float->signed casts discard the fractional component
    ///   - Float->unsigned casts act as if there was a hidden float->signed cast first.
    Cast(LocalIndex, Value),

    /// Binary operation
    BinOp(LocalIndex, Value, BinOp, Value),
    /// Unary operation
    UniOp(LocalIndex, UniOp, Value),
    /// Bitwise shift, different operation because it doesn't require equal types
    BitShift(LocalIndex, Value, BitShift, Value),

    /// Take a borrow/pointer to a local variable, with a flag indicating if it's a mutable borrow
    BorrowLocal(LocalIndex, bool, LocalIndex, WrapperList),
    /// Take a borrow/pointer to a global, with a flag indicating if it's a mutable borrow
    BorrowGlobal(LocalIndex, bool, AbsolutePath, WrapperList),
    /// Take a borrow/pointer based on an existing pointer, with flag indicating mutable
    PointerOffset(LocalIndex, bool, LocalIndex, WrapperList),
}
#[derive(Debug)]
pub enum BinOp {
    Add, Sub,
    Mul, Div, Rem,

    BitOr, BitAnd, BitXor,
}
#[derive(Debug)]
pub enum BitShift {
    Left,
    Right,
}
#[derive(Debug)]
pub enum UniOp {
    Not, Neg,
}
#[derive(Debug)]
pub struct JumpTarget {
    pub index: usize,
    pub args: Vec<LocalIndex>,
}
impl From<BlockIndex> for JumpTarget {
    fn from(value: BlockIndex) -> Self {
        JumpTarget { index: value.0, args: Vec::new() }
    }
}
#[derive(Debug)]
pub enum Terminator {
    /// Runtime assertion - unreachable code (generated by `match`)
    Unreachable,
    /// Return from the current function
    Return(Value),
    /// Jump immediately to another block
    Goto(JumpTarget),
    /// Compare two values and jump based on the result
    Compare {
        lhs: Value,
        op: CmpOp,
        rhs: Value,
        if_true: JumpTarget,
        if_false: JumpTarget,
    },
    /// Check if an enum (tagged union, or otherwise) is of a specific variant index
    MatchEnum {
        value: Value,
        index: usize,
        if_true: JumpTarget,
        if_false: JumpTarget,
    },
    /// Call a function by name
    CallPath {
        dst: LocalIndex,
        tgt: JumpTarget,
        path :crate::ast::path::AbsolutePath,
        args: Vec<Value>,
    },
    /// Call a function by pointer
    CallValue{
        dst: LocalIndex,
        tgt: JumpTarget,
        ptr: LocalIndex,
        args: Vec<Value>,
    },
}
#[derive(Debug)]
pub enum CmpOp {
    Eq,Ne,
    Lt,Le,
    Gt,Ge,
}

/// An index into the expression's list of blocks
#[derive(Clone, Copy)]
#[derive(Debug)]
#[derive(PartialEq,Eq)]
#[derive(PartialOrd,Ord)]
pub struct BlockIndex(pub usize);

/// A local variable/register index
#[derive(Clone, Copy)]
#[derive(Debug)]
#[derive(PartialEq,Eq)]
pub struct LocalIndex(pub usize);

/// A single basic-block (non-branching sequence of operations)
#[derive(Debug)]
pub struct Block
{
    /// Block arguments: List of locals populated by users of the block
    pub args: Vec<LocalIndex>,
    /// Statements evaluated in order
    pub statements: Vec<Operation>,
    /// Final statement, jumps to another block
    pub terminator: Terminator,
}

/// A lowered expression
#[derive(Debug)]
pub struct Expr
{
    pub locals: Vec<crate::ast::Type>,

    /// The list of basic blocks, block 0 is the entrypoint
    pub blocks: Vec<Block>,
}

impl Expr
{
    pub fn from_ast(parent: &mut super::State, expr_root: &crate::ast::ExprRoot, args: &[(crate::ast::Pattern, crate::ast::Type)]) -> Self {
        from_expr::from_ast(parent, expr_root, args)
    }
}

pub struct SsaExpr(Expr);
impl SsaExpr {
    pub fn new(e: Expr) -> Self {
        SsaExpr(ssa_ify::from_expr(e))
    }
    pub fn get(&self) -> &Expr {
        &self.0
    }
}

mod from_expr;
mod ssa_ify;
pub mod visit;

pub mod verify;

mod dump;
//pub use self::dump::dump;
pub use self::dump::dump_fcn;
pub use self::dump::dump_static;
