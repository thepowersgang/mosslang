
/// Address in generated IR
#[derive(Copy,Clone)]
pub struct Addr {
    pub block_idx: super::BlockIndex,
    pub stmt_idx: usize,
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
    match term {
    Terminator::Goto(_) => {},
    Terminator::Return(value) => visitor.reads_value(addr, value),
    Terminator::Compare(value_l, _, value_r, _, _) => {
        visitor.reads_value(addr, value_l);
        visitor.reads_value(addr, value_r);
    },
    Terminator::MatchEnum(value, _, _, _) => {
        visitor.reads_value(addr, value);
    },
    Terminator::CallPath(local_index_dst, _block_index, _absolute_path, values) => {
        for v in values {
            visitor.reads_value(addr, v);
        }
        visitor.writes_slot(addr, local_index_dst);
    }
    Terminator::CallValue(local_index_dst, _block_index, local_index_val, values) => {
        for v in values {
            visitor.reads_value(addr, v);
        }
        visitor.reads_slot(addr, local_index_val);
        visitor.writes_slot(addr, local_index_dst);
    },
    Terminator::Unreachable => {},
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
    Value::IntegerLiteral(_) => {},
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


#[derive(Copy,Clone)]
pub enum RouteEntry {
    /// Block's terminator reached
    PassedBlock(usize),
    /// Explicit stop requested by callback
    Stopped(Addr),
    /// Reached the same block again, route calculation stopped
    LoopedBlock(usize),
    /// A return terminator was seen
    Return,
}
pub fn enumerate_paths_from(ir: &super::Expr, addr: Addr, mut cb: impl FnMut(Vec<RouteEntry>)) {
    let mut stack = Vec::new();
    stack.push(( addr.block_idx.0, Vec::new(), ));
    while let Some((idx,mut vals)) = stack.pop() {
        if vals.iter().any(|v| match v { &RouteEntry::PassedBlock(other) if other == idx => true, _ => false }) {
            vals.push(RouteEntry::LoopedBlock(idx));
            cb(vals);
            continue
        }
        vals.push(RouteEntry::PassedBlock(idx));
        match &ir.blocks[idx].terminator {
        |super::Terminator::CallPath(_, block_index, _, _)
        |super::Terminator::CallValue(_, block_index, _, _)
        |super::Terminator::Goto(block_index)
         => {
            stack.push((block_index.0, vals));
        },
        |super::Terminator::Unreachable
        |super::Terminator::Return(_)
        => {
            vals.push(RouteEntry::Return);
            cb(vals)
        },
        |super::Terminator::Compare(_, _, _, block_true, block_false)
        |super::Terminator::MatchEnum(_, _, block_true, block_false)
        => {
            stack.push((block_true.0, vals.clone()));
            stack.push((block_false.0, vals));
        },
        }
    }
}