//! A simple IR like rust's MIR, to make codegen backend creation easier
//! 
//! Slightly simpler than MIR, as it doesn't have the complex LValue setup - all lvalues are just pointers
//!
//! 
//! Structure:
//! - List of basic blocks
// TODO: Should this be closer to LLVM IR (i.e. infinite register, with stack slots/alloca-s)
// - Registers being integers, pointers, or floats
// - For now: It's like mrustc's MIR - locals can be any type
use crate::ast::path::AbsolutePath;

/// A read-only value reference
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
    /// Deref a pointer and then apply wrappers
    Deref { ptr: LocalIndex, wrappers: WrapperList },
    /// String literal
    StringLiteral(crate::ast::StringLiteral),
    /// An integer literal
    IntegerLiteral(u128, ),
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
    /// Assign a value to a local variable
    AssignLocal(LocalIndex, Value),
    /// Assign a value to a dereference of a pointer (in a local variable)
    AssignDeref(LocalIndex, Value),

    /// Create an instance of a composite value (tuple or struct)
    CreateComposite(LocalIndex, Option<AbsolutePath>, Vec<Value>),

    /// Create an instance of a tagged-union enum
    CreateDataVariant(LocalIndex, AbsolutePath, usize, Vec<Value>),

    //// Cast a value to a different type (primitives only)
    //Cast(LocalIndex, Value),

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
pub enum Terminator {
    /// Jump immediately to another block
    Goto(BlockIndex),
    /// Return from the current function
    Return(Value),
    /// Compare two values and jump based on the result
    Compare(Value, CmpOp, Value, BlockIndex, BlockIndex),
    /// Check if an enum (tagged union, or otherwise) is of a specific variant index
    MatchEnum(Value, usize, BlockIndex, BlockIndex),
    /// Call a function by name
    CallPath(LocalIndex, BlockIndex, crate::ast::path::AbsolutePath, Vec<Value>,),
    /// Call a function by pointer
    CallValue(LocalIndex, BlockIndex, LocalIndex, Vec<Value>,),
    /// Runtime assertion - unreachable code (generated by `match`)
    Unreachable,
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
pub struct BlockIndex(pub usize);

/// A local variable index
#[derive(Clone, Copy)]
#[derive(Debug)]
pub struct LocalIndex(pub usize);

/// A single basic-block (non-branching sequence of operations)
#[derive(Debug)]
pub struct Block
{
    //pub args:
    pub statements: Vec<Operation>,
    pub terminator: Terminator,
}

/// A lowered expression
#[derive(Debug)]
pub struct Expr
{
    // TODO: Have different types of locals?
    // - Integer Registers
    // - Float Registers
    // - Pointer Registers
    // - Stack Slots
    pub locals: Vec<crate::ast::Type>,

    /// The list of basic blocks, block 0 is the entrypoint
    pub blocks: Vec<Block>,
}

pub fn from_expr(parent: &mut super::State, expr_root: &crate::ast::ExprRoot) -> Expr
{
    let mut expr_visit = from_expr::Visitor::new(parent, &expr_root.variables);
    let ret_val = expr_visit.visit_expr(&expr_root.e);
    expr_visit.finish(ret_val)
}
mod from_expr;

mod dump;
pub use self::dump::dump;
