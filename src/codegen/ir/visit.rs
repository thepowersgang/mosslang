
/// Address in generated IR
#[derive(Copy,Clone)]
#[derive(Debug)]
#[derive(PartialEq,Eq)]
#[derive(PartialOrd,Ord)]
pub struct Addr {
    pub block_idx: super::BlockIndex,
    pub stmt_idx: usize,
}
impl ::std::fmt::Display for Addr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "BB{}/", self.block_idx.0)?;
        if self.stmt_idx == !0 {
            f.write_str("T")
        }
        else {
            write!(f, "{}", self.stmt_idx)
        }
    }
}

pub trait Visitor {
    fn block(&mut self, idx: super::BlockIndex, b: &super::Block) {
        visit_block(self, idx, b);
    }
    fn operation(&mut self, addr: Addr, op: &super::Operation) {
        visit_operation(self, addr, op);
    }
    fn terminator(&mut self, addr: Addr, term: &super::Terminator) {
        visit_terminator(self, addr, term);
    }
    
    fn writes_slot(&mut self, addr: Addr, local_index: &super::LocalIndex) {
        let _ = (addr, local_index);
    }
    fn reads_slot(&mut self, addr: Addr, local_index: &super::LocalIndex) {
        let _ = (addr, local_index);
    }
    fn reads_wrappers(&mut self, addr: Addr, v: &super::WrapperList) {
        visit_wrapper(self, addr, v)
    }
    fn reads_value(&mut self, addr: Addr, v: &super::Value) {
        visit_value(self, addr, v)
    }
}

pub fn visit_expr<V>(v: &mut V, ir: &super::Expr)
where
    V: ?Sized + Visitor,
{
    for (block_idx,block) in ir.blocks.iter().enumerate() {
        v.block(super::BlockIndex(block_idx), block)
    }
}
pub fn visit_block<V>(visitor: &mut V, block_idx: super::BlockIndex, b: &super::Block)
where
    V: ?Sized + Visitor,
{
    for (stmt_idx,op) in b.statements.iter().enumerate() {
        visitor.operation(Addr { block_idx, stmt_idx }, op);
    }
    visitor.terminator(Addr { block_idx, stmt_idx: !0 }, &b.terminator);
}
pub fn visit_operation<V>(visitor: &mut V, addr: Addr, op: &super::Operation)
where
    V: ?Sized + Visitor,
{
    use super::Operation;
    match op {
    Operation::Alloca { dst, ty: _ } => visitor.writes_slot(addr, dst),
    Operation::AssignDeref(ptr, v) => {
        visitor.reads_slot(addr, ptr);
        visitor.reads_value(addr, v);
    },
    Operation::BorrowLocal(local_index_dst, _is_mut, local_index_src, wrappers) => {
        visitor.reads_slot(addr, local_index_src); // Shouldn't matter, as we ignore borrowed locals
        visitor.reads_wrappers(addr, wrappers);
        visitor.writes_slot(addr, local_index_dst);
    },

    Operation::AssignLocal(local_index, v)
    |Operation::UniOp(local_index, _, v) => {
        visitor.reads_value(addr, v);
        visitor.writes_slot(addr, local_index);
    },
    Operation::CreateComposite(local_index, _, values)
    |Operation::CreateDataVariant(local_index, _, _, values) => {
        for v in values {
            visitor.reads_value(addr, v);
        }
        visitor.writes_slot(addr, local_index);
    },
    Operation::BinOp(local_index, vl, _, vr)
    |Operation::BitShift(local_index, vl, _, vr) => {
        visitor.reads_value(addr, vl);
        visitor.reads_value(addr, vr);
        visitor.writes_slot(addr, local_index);
    },
    Operation::BorrowGlobal(local_index, _, _, wrappers) => {
        visitor.reads_wrappers(addr, wrappers);
        visitor.writes_slot(addr, local_index);
    },
    Operation::PointerOffset(local_index, _, ptr, wrappers) => {
        visitor.reads_slot(addr, ptr);
        visitor.reads_wrappers(addr, wrappers);
        visitor.writes_slot(addr, local_index);
    },
    }
}

pub fn visit_terminator<V>(visitor: &mut V, addr: Addr, term: &super::Terminator)
where
    V: ?Sized + Visitor,
{
    use crate::codegen::ir::Terminator;
    fn visit_target<V: ?Sized + Visitor>(visitor: &mut V, addr: Addr, tgt: &super::JumpTarget) {
        for arg in &tgt.args {
            visitor.reads_slot(addr, arg);
        }
    }
    match term {
    Terminator::Unreachable => {},
    Terminator::Goto(tgt) => {
        visit_target(visitor, addr, tgt);
    },
    Terminator::Return(value) => visitor.reads_value(addr, value),
    Terminator::Compare { lhs: value_l, op: _, rhs: value_r, if_false, if_true } => {
        visitor.reads_value(addr, value_l);
        visitor.reads_value(addr, value_r);
        visit_target(visitor, addr, if_false);
        visit_target(visitor, addr, if_true);
    },
    Terminator::MatchEnum { value, if_false, if_true, .. } => {
        visitor.reads_value(addr, value);
        visit_target(visitor, addr, if_false);
        visit_target(visitor, addr, if_true);
    },
    Terminator::CallPath { dst, path: _, args, tgt } => {
        for v in args {
            visitor.reads_value(addr, v);
        }
        visitor.writes_slot(addr, dst);
        visit_target(visitor, addr, tgt);
    },
    Terminator::CallValue { dst, ptr, args, tgt } => {
        for v in args {
            visitor.reads_value(addr, v);
        }
        visitor.reads_slot(addr, ptr);
        visitor.writes_slot(addr, dst);
        visit_target(visitor, addr, tgt);
    },
    }
}

/// Visit value reads in a list of field-access wrappers (indexing operations)
pub fn visit_wrapper<V: ?Sized + Visitor>(visitor: &mut V, addr: Addr, v: &super::WrapperList) {
    for w in v.iter() {
        match  w {
        super::Wrapper::Field(_) => {},
        super::Wrapper::IndexBySlot(local_index) => visitor.reads_slot(addr, &local_index),
        }
    }
}
/// Visit slot components of a value
pub fn visit_value<V: ?Sized + Visitor>(visitor: &mut V, addr: Addr, v: &super::Value) {
    use crate::codegen::ir::Value;
    match v {
    Value::Unreachable => {},
    Value::ImplicitUnit => {},
    Value::StringLiteral(_) => {},
    Value::IntegerLiteral(..) => {},
    Value::FunctionPointer(_,_) => {},

    Value::Local(local_index, wrapper_list) => {
        visitor.reads_slot(addr, local_index);
        visitor.reads_wrappers(addr, wrapper_list);
    },
    Value::Named(_, wrapper_list) => {
        visitor.reads_wrappers(addr, wrapper_list);
    },
    Value::Deref { ptr, wrappers } => {
        visitor.reads_slot(addr, ptr);
        visitor.reads_wrappers(addr, wrappers);
    },
    }
}

pub trait VisitorMut {
    fn block(&mut self, idx: super::BlockIndex, b: &mut super::Block) {
        visit_block_mut(self, idx, b);
    }
    fn operation(&mut self, addr: Addr, op: &mut super::Operation) {
        visit_operation_mut(self, addr, op);
    }
    fn terminator(&mut self, addr: Addr, term: &mut super::Terminator) {
        visit_terminator_mut(self, addr, term);
    }
    
    fn writes_slot(&mut self, addr: Addr, local_index: &mut super::LocalIndex) {
        let _ = (addr, local_index);
    }
    fn reads_slot(&mut self, addr: Addr, local_index: &mut super::LocalIndex) {
        let _ = (addr, local_index);
    }
    fn reads_wrappers(&mut self, addr: Addr, v: &mut super::WrapperList) {
        visit_wrappers_mut(self, addr, v)
    }
    fn reads_value(&mut self, addr: Addr, v: &mut super::Value) {
        visit_value_mut(self, addr, v)
    }
}
pub fn visit_expr_mut<V: ?Sized + VisitorMut>(v: &mut V, ir: &mut super::Expr)
{
    for (block_idx,block) in ir.blocks.iter_mut().enumerate() {
        v.block(super::BlockIndex(block_idx), block)
    }
}
pub fn visit_block_mut<V: ?Sized + VisitorMut>(visitor: &mut V, block_idx: super::BlockIndex, b: &mut super::Block)
{
    for (stmt_idx,op) in b.statements.iter_mut().enumerate() {
        visitor.operation(Addr { block_idx, stmt_idx }, op);
    }
    visitor.terminator(Addr { block_idx, stmt_idx: !0 }, &mut b.terminator);
}
pub fn visit_operation_mut<V: ?Sized + VisitorMut,>(visitor: &mut V, addr: Addr, op: &mut super::Operation)
{
    use super::Operation;
    match op {
    Operation::Alloca { dst, ty: _ } => visitor.writes_slot(addr, dst),
    Operation::AssignDeref(ptr, v) => {
        visitor.reads_slot(addr, ptr);
        visitor.reads_value(addr, v);
    },
    Operation::BorrowLocal(local_index_dst, _is_mut, local_index_src, wrappers) => {
        visitor.reads_slot(addr, local_index_src); // Shouldn't matter, as we ignore borrowed locals
        visitor.reads_wrappers(addr, wrappers);
        visitor.writes_slot(addr, local_index_dst);
    },

    Operation::AssignLocal(local_index, v)
    |Operation::UniOp(local_index, _, v) => {
        visitor.reads_value(addr, v);
        visitor.writes_slot(addr, local_index);
    },
    Operation::CreateComposite(local_index, _, values)
    |Operation::CreateDataVariant(local_index, _, _, values) => {
        for v in values {
            visitor.reads_value(addr, v);
        }
        visitor.writes_slot(addr, local_index);
    },
    Operation::BinOp(local_index, vl, _, vr)
    |Operation::BitShift(local_index, vl, _, vr) => {
        visitor.reads_value(addr, vl);
        visitor.reads_value(addr, vr);
        visitor.writes_slot(addr, local_index);
    },
    Operation::BorrowGlobal(local_index, _, _, wrappers) => {
        visitor.reads_wrappers(addr, wrappers);
        visitor.writes_slot(addr, local_index);
    },
    Operation::PointerOffset(local_index, _, ptr, wrappers) => {
        visitor.reads_slot(addr, ptr);
        visitor.reads_wrappers(addr, wrappers);
        visitor.writes_slot(addr, local_index);
    },
    }
}

pub fn visit_terminator_mut<V: ?Sized + VisitorMut>(visitor: &mut V, addr: Addr, term: &mut super::Terminator)
{
    use crate::codegen::ir::Terminator;
    match term {
    Terminator::Unreachable => {},
    Terminator::Goto(_) => {},
    Terminator::Return(value) => visitor.reads_value(addr, value),
    Terminator::Compare { lhs: value_l, rhs: value_r, .. } => {
        visitor.reads_value(addr, value_l);
        visitor.reads_value(addr, value_r);
    },
    Terminator::MatchEnum { value, .. } => {
        visitor.reads_value(addr, value);
    },
    Terminator::CallPath { dst, path: _, args, tgt: _ } => {
        for v in args {
            visitor.reads_value(addr, v);
        }
        visitor.writes_slot(addr, dst);
    },
    Terminator::CallValue { dst, ptr, args, tgt: _} => {
        for v in args {
            visitor.reads_value(addr, v);
        }
        visitor.reads_slot(addr, ptr);
        visitor.writes_slot(addr, dst);
    },
    }
}

/// Visit value reads in a list of field-access wrappers (indexing operations)
pub fn visit_wrappers_mut<V: ?Sized + VisitorMut>(visitor: &mut V, addr: Addr, v: &mut super::WrapperList) {
    let mut reset = None;
    for (i,w) in v.iter().enumerate() {
        match w {
        super::Wrapper::Field(_) => {},
        super::Wrapper::IndexBySlot(local_index) => {
            // NOTE: THis is a little hacky, avoids creating a new wrapper list until something changes
            let mut tmp = local_index;
            visitor.reads_slot(addr, &mut tmp);

            if tmp != local_index {
                reset = Some((i, tmp));
                break
            }
        },
        }
    }

    if let Some((i, tmp)) = reset {
        let mut new = super::WrapperList::default();
        let mut it = v.iter();
        for _ in 0 .. i {
            new.push( it.next().unwrap() );
        }
        new.push(super::Wrapper::IndexBySlot(tmp));
        let _ = it.next().unwrap();
        for w in it {
            new.push(match w {
            super::Wrapper::Field(_) => w,
            super::Wrapper::IndexBySlot(mut local_index) => {
                visitor.reads_slot(addr, &mut local_index);
                super::Wrapper::IndexBySlot(local_index)
            }
            });
        }

        *v = new;
    }
}
/// Visit slot components of a value
pub fn visit_value_mut<V: ?Sized + VisitorMut>(visitor: &mut V, addr: Addr, v: &mut super::Value) {
    use crate::codegen::ir::Value;
    match v {
    Value::Unreachable => {},
    Value::ImplicitUnit => {},
    Value::StringLiteral(_) => {},
    Value::IntegerLiteral(..) => {},
    Value::FunctionPointer(_,_) => {},

    Value::Local(local_index, wrapper_list) => {
        visitor.reads_slot(addr, local_index);
        visitor.reads_wrappers(addr, wrapper_list);
    },
    Value::Named(_, wrapper_list) => {
        visitor.reads_wrappers(addr, wrapper_list);
    },
    Value::Deref { ptr, wrappers } => {
        visitor.reads_slot(addr, ptr);
        visitor.reads_wrappers(addr, wrappers);
    },
    }
}

pub struct Route(Vec<usize>,RouteExit);
impl Route {
    /// Returns a tri-state: Yes, No, Looped
    pub fn contains(&self, tgt_addr: &Addr) -> Option<bool> {
        for &bb in self.0.iter() {
            if bb == tgt_addr.block_idx.0 {
                return Some(true)
            }
        }
        match self.1 {
        RouteExit::Stopped(addr) => Some(addr.block_idx.0 == tgt_addr.block_idx.0 && addr.stmt_idx >= tgt_addr.stmt_idx),
        RouteExit::LoopedBlock(_) => None,
        RouteExit::Return => Some(false),
        }
    }

    /// Iterate over blocks entered in this route (doesn't include the starting block)
    pub fn blocks(&self) -> impl Iterator<Item=usize> + '_ {
        self.0.iter().copied()
            .chain(match self.1 {
                RouteExit::Stopped(addr) => Some(addr.block_idx.0),
                RouteExit::LoopedBlock(b) => Some(b),
                RouteExit::Return => None,
            }.into_iter())
    }
}
impl ::std::fmt::Debug for Route {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for e in self.0.iter() {
            write!(f, "{}->", e)?;
        }
        match self.1 {
        RouteExit::Stopped(addr) => write!(f,"stop@{}", addr)?,
        RouteExit::LoopedBlock(block) => write!(f, "loop{}", block)?,
        RouteExit::Return => f.write_str("exit")?,
        }
        write!(f, "]")?;
        Ok(())
    }
}
#[derive(Copy,Clone)]
#[derive(Debug)]
pub enum RouteExit {
    /// Explicit stop requested by callback
    Stopped(Addr),
    /// Reached the same block again, route calculation stopped
    LoopedBlock(usize),
    /// A return terminator was seen
    Return,
}
pub fn enumerate_paths_from(ir: &super::Expr, addr: Addr, stop: impl Fn(Addr)->bool, mut cb: impl FnMut(Route)) {
    let mut stack = Vec::new();
    stack.push(( addr.block_idx.0, Vec::new(), ));
    while let Some((idx,mut vals)) = stack.pop() {
        if vals.iter().any(|&other| other == idx) {
            cb(Route(vals,RouteExit::LoopedBlock(idx)));
            continue
        }
        // Step through
        fn check_exit(ir: &super::Expr, start: Addr, idx: usize, stop: &impl Fn(Addr)->bool) -> Option<Addr> {
            for stmt_idx in (if idx == start.block_idx.0 { start.stmt_idx.saturating_add(1) } else { 0 }) .. ir.blocks[idx].statements.len() {
                let a = Addr { block_idx: super::BlockIndex(idx), stmt_idx };
                if stop(a) {
                    return Some(a);
                }
            }
            if idx != start.block_idx.0 || start.stmt_idx != !0 {
                let a = Addr { block_idx: super::BlockIndex(idx), stmt_idx: !0 };
                if stop(a) {
                    return Some(a);
                }
            }
            None
        }
        if let Some(a) = check_exit(ir, addr, idx, &stop) {
            cb(Route(vals, RouteExit::Stopped(a)));
            continue
        }
        vals.push(idx);
        match &ir.blocks[idx].terminator {
        |super::Terminator::CallPath { tgt, .. }
        |super::Terminator::CallValue { tgt, .. }
        |super::Terminator::Goto(tgt)
         => {
            stack.push((tgt.index, vals));
        },
        |super::Terminator::Unreachable
        |super::Terminator::Return(_)
        => {
            cb(Route(vals, RouteExit::Return))
        },
        |super::Terminator::Compare { if_true, if_false, .. }
        |super::Terminator::MatchEnum { if_true, if_false, .. }
        => {
            stack.push((if_true.index, vals.clone()));
            stack.push((if_false.index, vals));
        },
        }
    }
}