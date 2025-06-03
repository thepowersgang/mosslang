//! Convert an IR expression into SSA form
//! 
//! Same underlying representation, but uses `Operation::CreateSlot`
use crate::INDENT;
use super::{Operation,Terminator};
use super::visit::VisitorMut;

pub fn from_expr(mut ir: super::Expr) -> super::Expr
{
    let _i = INDENT.inc("ssa_ify");
    // Allocas are needed if:
    // - The variable is written twice
    // - The variable is borrowed
    let mut borrowed = BitSet::new(ir.locals.len());
    let mut written = BitSet::new(ir.locals.len());
    let mut twice_written = BitSet::new(ir.locals.len());
    for block in &ir.blocks {
        for stmt in &block.statements {
            let dst = match stmt {
                Operation::AssignDeref(_, _) => continue,
                Operation::BorrowLocal(local_index_dst, _is_mut, local_index_src, _wrappers) => {
                    borrowed.set(local_index_src.0);
                    local_index_dst
                },

                |Operation::Alloca { dst: local_index, .. }
                |Operation::AssignLocal(local_index, _)
                |Operation::CreateComposite(local_index, _, _)
                |Operation::CreateDataVariant(local_index, _, _, _)
                |Operation::BinOp(local_index, _, _, _) 
                |Operation::UniOp(local_index, _, _)
                |Operation::BitShift(local_index, _, _, _)
                |Operation::BorrowGlobal(local_index, _, _, _)
                |Operation::PointerOffset(local_index, _, _, _) => local_index
                };
            if written.set(dst.0) {
                twice_written.set(dst.0);
            }
        }
    }
    
    // For all of the multi-write values (and not borrowed), if they can be trivially turned into block params instead of making allocas
    // - If all writes jump to the same block
    // - TODO: More complex versions - generate paths between writes and reads (de-duplicated to shortest).
    //   > Find common point and inject block params there
    for slot in 0 .. ir.locals.len() {
        if twice_written.is_set(slot) && !borrowed.is_set(slot) {
            struct ReadsState {
                reads: Vec<super::visit::Addr>,
                writes: Vec<super::visit::Addr>,
                slot: usize,
            }
            impl super::visit::Visitor for ReadsState {
                fn writes_slot(&mut self, addr: super::visit::Addr, local_index: &super::LocalIndex) {
                    if local_index.0 == self.slot {
                        self.writes.push(addr);
                    }
                }
                fn reads_slot(&mut self, addr: super::visit::Addr, local_index: &super::LocalIndex) {
                    if local_index.0 == self.slot {
                        self.reads.push(addr);
                    }
                }
            }
            let mut rs = ReadsState {
                reads: Vec::new(),
                writes: Vec::new(),
                slot,
            };
            // Enumerate reads and writes
            super::visit::visit_expr(&mut rs, &ir);

            // Bitset of blocks that are shared between paths
            // - At each of these, a new local needs to be made
            let mut common_blocks = BitSet::new(ir.blocks.len());
            // Routes from writes already processed
            let mut other_write_routes: Vec<super::visit::Route> = Vec::new();

            let mut remap_table = ::std::collections::BTreeMap::<super::visit::Addr,super::LocalIndex>::new();

            if rs.writes.windows(2).any(|w| w[0].block_idx == w[1].block_idx) {
                // Only want to keep the second, the first should just get a new local allocated and an entry added to the remap table
                todo!("Handle multiple writes in one block");
            }

            // Enumerate all paths from each write point
            // - Stop enumerating when another write or read position is seen
            // Find common blocks between the writes
            // - Tag that common block as being a merge point
            for w in rs.writes.iter() {
                let path_count = other_write_routes.len();

                // Enumerate all routes a write could take
                super::visit::enumerate_paths_from(&ir, *w, |a| {
                    rs.reads.contains(&a) || (a != *w && rs.writes.contains(&a))
                },
                |route| {
                    let mut is_read = false;
                    let mut maybe_read = false;
                    for r in rs.reads.iter() {
                        match route.contains(r) {
                        Some(true) => is_read = true,
                        Some(false) => {},
                        None => maybe_read = true,
                        }
                    }

                    println!("{INDENT}#{slot} @{w} route = {:?} {}", route, if is_read { "read" } else if maybe_read { "maybe" } else { "unread" });
                    // Don't consider if this path doesn't read the value
                    if !(is_read || maybe_read) {
                    }
                    else {
                        // Check if there's a shared block with one already in the list
                        for path in other_write_routes[..path_count].iter() {
                            for bb_idx in route.blocks() {
                                if path.blocks().any(|v| v == bb_idx) {
                                    // Found a common point
                                    if !common_blocks.set(bb_idx) {
                                        println!("{INDENT}#{slot} Arms join at BB{bb_idx}")
                                    }
                                    break;
                                }
                            }
                        }

                        // Add to list
                        other_write_routes.push(route);
                    }
                });
            }

            // Allocate a new local for every write, and one for all but one of the union/common blocks
            let mut remap_slot = Some(super::LocalIndex(slot));
            for block_idx in 0 .. ir.blocks.len() {
                if common_blocks.is_set(block_idx) {
                    // Add an argument (TODO: Different index if there's multiple common blocks)
                    let v = remap_slot.take().unwrap_or_else(|| {
                        let ty = ir.locals[slot].clone();
                        let rv = super::LocalIndex(ir.locals.len());
                        ir.locals.push(ty);
                        rv
                    });
                    ir.blocks[block_idx].args.push(v);
                    remap_table.insert(super::visit::Addr { block_idx: super::BlockIndex(block_idx), stmt_idx: 0}, v);
                }
            }
            for &w in &rs.writes {
                let ty = ir.locals[slot].clone();
                remap_table.insert(w, super::LocalIndex(ir.locals.len()));
                ir.locals.push(ty);
            }
            // Propagate entries in the remap table
            {
                let mut exit_table: Vec<_> = (0 .. ir.blocks.len()).map(|_| None).collect();
                for (a,v) in remap_table.iter() {
                    // Since `remap_table` abvoe is a BTreeMap, it's sorted - so later remaps come first. This means that if we overwrite the `exit_table` entry it'll be correct
                    exit_table[a.block_idx.0] = Some(*v);
                }
                let mut stack = Vec::new();
                // Prime the stack with the block exits
                for (block_idx,local) in exit_table.iter().enumerate() {
                    if let Some(local) = local {
                        stack.push((block_idx, *local));
                    }
                }
                while let Some((block_idx, local)) = stack.pop() {
                    let mut set = |tgt: &mut super::JumpTarget| {
                        if common_blocks.is_set(tgt.index) && tgt.args.last() != Some(&local) {
                            tgt.args.push(local);
                        }
                        else {
                            remap_table.insert(super::visit::Addr { block_idx: super::BlockIndex(tgt.index), stmt_idx: 0}, local);
                        }
                        if exit_table[tgt.index].is_none() {
                            exit_table[tgt.index] = Some(local);
                            stack.push((tgt.index, local));
                        }
                    };
                    match &mut ir.blocks[block_idx].terminator {
                    Terminator::Unreachable => {}
                    Terminator::Return(_) => {}
                    Terminator::Goto(tgt)
                    |Terminator::CallPath { tgt, .. }
                    |Terminator::CallValue { tgt, .. } => set(tgt),
                    Terminator::Compare { if_true, if_false, .. }
                    |Terminator::MatchEnum { if_true, if_false, .. } => {
                        set(if_true);
                        set(if_false);
                    },
                    }
                }
            }
            // Apply the remap table
            struct V {
                slot: usize,
                remap_table: ::std::collections::BTreeMap<super::visit::Addr,super::LocalIndex>,
            }
            impl VisitorMut for V {
                fn writes_slot(&mut self, addr: super::visit::Addr, local_index: &mut super::LocalIndex) {
                    if local_index.0 == self.slot {
                        //*local_index = *self.remap_table.range(..=addr).last().unwrap().1
                        *local_index = self.remap_table[&addr];
                    }   
                }
                fn reads_slot(&mut self, addr: super::visit::Addr, local_index: &mut super::LocalIndex) {
                    if local_index.0 == self.slot {
                        let n = *self.remap_table.range(..addr).last().unwrap().1;
                        println!("{INDENT}reads_slot: @{addr} {local_index:?} -> {n:?}");
                        *local_index = n;
                    }   
                }
            }
            super::visit::visit_expr_mut(&mut V { slot, remap_table }, &mut ir);
        }
    }

    // For borrowed, inject an Alloca at start of function
    let mut alloca_needed = BitSet::new(ir.locals.len());
    let mut new_ops = Vec::new();
    for slot in 0 .. ir.locals.len() {
        if borrowed.is_set(slot) {
            alloca_needed.set(slot);
            new_ops.push(super::Operation::Alloca { dst: super::LocalIndex(slot), ty: ir.locals[slot].clone() });
        }
    }
    
    for (block_idx,block) in ir.blocks.iter_mut().enumerate() {
        for (stmt_idx,stmt) in block.statements.iter_mut().enumerate() {
            struct V<'a> {
                alloca_needed: &'a BitSet,
            }
            impl VisitorMut for V<'_> {
                fn writes_slot(&mut self, _: super::visit::Addr, local_index: &mut super::LocalIndex) {
                    if self.alloca_needed.is_set(local_index.0) {
                        todo!("update_write: Add a `_I = _new` after this statement");
                    }   
                }
                fn reads_slot(&mut self, _: super::visit::Addr, local_index: &mut super::LocalIndex) {
                    if self.alloca_needed.is_set(local_index.0) {
                        todo!("update_use: Add a `_new = _I` before this statement");
                    }   
                }

                fn reads_value(&mut self, addr: super::visit::Addr, value: &mut super::Value) {
                    use crate::codegen::ir::Value;
                    match value {
                    Value::Local(local_index, wrapper_list) => {
                        self.reads_wrappers(addr, wrapper_list);
                        if self.alloca_needed.is_set(local_index.0) {
                            *value = Value::Deref { ptr: *local_index, wrappers: ::std::mem::take(wrapper_list) };
                        }
                    },
                    _ => super::visit::visit_value_mut(self, addr, value),
                    }
                }
            }
            let addr = super::visit::Addr { block_idx: super::BlockIndex(block_idx), stmt_idx };
            let mut v = V { alloca_needed: &alloca_needed };
            match stmt {
            Operation::AssignLocal(local_index, value) => {
                v.reads_value(addr, value);
                if alloca_needed.is_set(local_index.0) {
                    *stmt = Operation::AssignDeref(*local_index, ::std::mem::replace(value, super::Value::Unreachable));
                }
            },
            Operation::BorrowLocal(local_index_dst, flag, local_index_src, wrapper_list) => {
                v.writes_slot(addr, local_index_dst);
                v.reads_wrappers(addr, wrapper_list);
                if alloca_needed.is_set(local_index_src.0) {
                    *stmt = Operation::PointerOffset(*local_index_dst, *flag, *local_index_src, ::std::mem::take(wrapper_list));
                }
            },
            _ => {
                super::visit::visit_operation_mut(&mut v, addr, stmt);
            },
            }
        }
    }
    new_ops.append(&mut ir.blocks[0].statements);
    ir.blocks[0].statements = new_ops;

    ir
}

/// Quick bitset implementation
struct BitSet {
    v: Vec<u32>,
}
impl BitSet {
    fn new(count: usize) -> Self {
        BitSet { v: vec![0; (count + 31) / 32] }
    }
    fn is_set(&self, slot: usize) -> bool {
        if slot >= self.v.len() * 32 {
            false
        }
        else {
            self.v[slot / 32] & (1 << (slot % 32)) != 0
        }
    }
    /// Returns the previous state of the entry
    fn set(&mut self, slot: usize) -> bool {
        let rv = self.is_set(slot);
        self.v[slot / 32] |= 1 << (slot % 32);
        rv
    }
}
