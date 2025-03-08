//! A simple IR like rust's MIR, to make codegen backend creation easier

#[derive(Debug)]
pub enum Value {
    Unreachable,
    ImplicitUnit,
    Local(LocalIndex, WrapperList),
    Named(crate::ast::path::AbsolutePath, WrapperList),
    StringLiteral(crate::ast::StringLiteral),
    IntegerLiteral(u128, ),
}
impl Value {
    fn field(&self, idx: usize) -> Value {
        match self {
        Value::Unreachable => Value::Unreachable,
        Value::ImplicitUnit => panic!("Field on void"),
        Value::StringLiteral(..) => panic!("Field on string"),
        Value::IntegerLiteral(..) => panic!("Field on integer"),
        Value::Local(root, indexes) => Value::Local(*root, indexes.with_added(Wrapper::Field(idx))),
        Value::Named(root, indexes) => Value::Named(root.clone(), indexes.with_added(Wrapper::Field(idx))),
        }
    }
}
#[derive(Default)]
struct WrapperList(Vec<u32>);
impl WrapperList {
    pub fn new() -> WrapperList {
        WrapperList(Vec::new())
    }
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
    pub fn with_added(&self, w: Wrapper) -> Self {
        WrapperList(self.0.iter().copied()
            .chain(::std::iter::once(Self::encode(w)))
            .collect())
    }
    pub fn add(mut self, w: Wrapper) -> Self {
        self.push(w);
        self
    }
    pub fn push(&mut self, w: Wrapper) {
        self.0.push(Self::encode(w));
    }

    fn encode(w: Wrapper) -> u32 {
        match w {
        Wrapper::Deref => !0,
        Wrapper::Field(idx) => idx as u32,
        Wrapper::IndexBySlot(LocalIndex(s)) => (1 << 31) | s as u32,
        }
    }

    fn decode(v: u32) -> Wrapper {
        if v == !0 {
            Wrapper::Deref
        }
        else if v & 1 << 31 == 0 {
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
enum Wrapper {
    Deref,
    Field(usize),
    IndexBySlot(LocalIndex),
}
#[derive(Debug)]
pub enum Operation {
    AssignLocal(LocalIndex, WrapperList, Value),
    AssignNamed(crate::ast::path::AbsolutePath, Value),
    //CreateLiteral(LocalIndex, ),
    BinOp(LocalIndex, Value, BinOp, Value),
    UniOp(LocalIndex, UniOp, Value),
    //Compare(LocalIndex, Value, CmpOp, Value),
}
#[derive(Debug)]
pub enum BinOp {
    Add, Sub,
    Mul, Div, Rem,
}
#[derive(Debug)]
pub enum UniOp {
    Not, Neg,
}
#[derive(Debug)]
pub enum Terminator {
    Goto(BlockIndex),
    Return(Value),
    Compare(Value, CmpOp, Value, BlockIndex, BlockIndex),
    CallPath(LocalIndex, BlockIndex, crate::ast::path::AbsolutePath, Vec<Value>,),
}
#[derive(Debug)]
pub enum CmpOp {
    Eq,Ne,
    Lt,Le,
    Gt,Ge,
}

#[derive(Clone, Copy)]
#[derive(Debug)]
pub struct BlockIndex(pub usize);
#[derive(Clone, Copy)]
#[derive(Debug)]
pub struct LocalIndex(pub usize);

pub struct Block
{
    //pub args:
    pub statements: Vec<Operation>,
    pub terminator: Terminator,
}
pub struct Expr
{
    pub locals: Vec<crate::ast::Type>,
    pub blocks: Vec<Block>,
}

pub fn from_expr(parent: &mut super::State, expr_root: &crate::ast::ExprRoot) -> Expr
{
    let mut expr_visit = from_expr::Visitor::new(parent, &expr_root.variables);
    let ret_val = expr_visit.visit_expr(&expr_root.e);
    todo!();
}
mod from_expr;