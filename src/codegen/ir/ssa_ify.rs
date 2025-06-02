//! Convert an IR expression into SSA form
//! 
//! Same underlying representation, but uses `Operation::CreateSlot`
use crate::INDENT;
use super::Operation;

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

                Operation::AssignLocal(local_index, _)
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

            // Enumerate all paths from each write point
            // - Stop enumerating when another write or read position is seen
            // Find common blocks between the writes
            // - Tag that common block as being a merge point
            for w in rs.writes.iter() {
                let path_count = other_write_routes.len();
                // TODO: Check if there's another write in the same block, if there is then allocate a new local for the first one

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

            // TODO: If there's multiple common blocks, then this logic gets harder
            assert!( (0 .. ir.blocks.len()).map(|v| common_blocks.is_set(v)).count() <= 1 );

            // Allocate a new local for every write, and one for all but one of the union/common blocks
            for block_idx in 0 .. ir.blocks.len() {
                if common_blocks.is_set(block_idx) {
                    // Add an argument (TODO: Different index if there's multiple common blocks)
                    ir.blocks[block_idx].args.push(super::LocalIndex(slot));
                }
            }
            todo!("Multi-write #{}", slot);
        }
    }

    // For borrowed, inject an Alloca at start of function
    let mut alloca_needed = BitSet::new(ir.locals.len());
    for slot in 0 .. ir.locals.len() {
        if borrowed.is_set(slot) {
            alloca_needed.set(slot);
        }
    }
    
    for block in ir.blocks.iter_mut() {
        for stmt in block.statements.iter_mut() {
            fn update_write(alloca_needed: &BitSet, local_index: &mut super::LocalIndex) {
                if alloca_needed.is_set(local_index.0) {
                    todo!("update_write: Add a `_I = _new` after this statement");
                }
            }
            fn update_use(alloca_needed: &BitSet, local_index: &mut super::LocalIndex) {
                if alloca_needed.is_set(local_index.0) {
                    todo!("update_use: Add a `_new = _I` before this statement");
                }
            }
            fn update_wrappers(alloca_needed: &BitSet, wrapper_list: &mut super::WrapperList) {
                for w in wrapper_list.iter() {
                    match w {
                    super::Wrapper::IndexBySlot(mut s) if alloca_needed.is_set(s.0) => {
                        update_use(alloca_needed, &mut s);
                        todo!()
                    },
                    _ => {},
                    }
                }
            }
            fn update_value(alloca_needed: &BitSet, value: &mut super::Value) {
                use crate::codegen::ir::Value;
                match value {
                Value::Unreachable => {},
                Value::ImplicitUnit => {},
                Value::StringLiteral(_) => {},
                Value::IntegerLiteral(_) => {},
                Value::FunctionPointer(_, _) => {},

                Value::Local(local_index, wrapper_list) => {
                    update_wrappers(alloca_needed, wrapper_list);
                    if alloca_needed.is_set(local_index.0) {
                        *value = Value::Deref { ptr: *local_index, wrappers: ::std::mem::take(wrapper_list) };
                    }
                },
                Value::Named(_, wrapper_list) => {
                    update_wrappers(alloca_needed, wrapper_list);
                },
                Value::Deref { ptr, wrappers } => {
                    update_wrappers(alloca_needed, wrappers);
                    update_use(alloca_needed, ptr);
                },
                }
            }
            match stmt {
            Operation::AssignLocal(local_index, value) => {
                update_value(&alloca_needed, value);
                if alloca_needed.is_set(local_index.0) {
                    *stmt = Operation::AssignDeref(*local_index, ::std::mem::replace(value, super::Value::Unreachable));
                }
            },
            Operation::AssignDeref(local_index, value) => {
                update_use(&alloca_needed, local_index);
                update_value(&alloca_needed, value);
            }
            Operation::CreateComposite(local_index, _, values) => {
                update_use(&alloca_needed, local_index);
                for value in values {
                    update_value(&alloca_needed, value);
                }
            }
            Operation::CreateDataVariant(local_index, _, _, values) => {
                update_use(&alloca_needed, local_index);
                for value in values {
                    update_value(&alloca_needed, value);
                }
            }
            Operation::BinOp(local_index_dst, value_l, _, value_r)
            |Operation::BitShift(local_index_dst, value_l, _, value_r) => {
                update_write(&alloca_needed, local_index_dst);
                update_value(&alloca_needed, value_l);
                update_value(&alloca_needed, value_r);
            }
            Operation::UniOp(local_index_dst, _, value) => {
                update_write(&alloca_needed, local_index_dst);
                update_value(&alloca_needed, value);
            }
            Operation::BorrowLocal(local_index_dst, flag, local_index_src, wrapper_list) => {
                update_write(&alloca_needed, local_index_dst);
                update_wrappers(&alloca_needed, wrapper_list);
                if alloca_needed.is_set(local_index_src.0) {
                    *stmt = Operation::PointerOffset(*local_index_dst, *flag, *local_index_src, ::std::mem::take(wrapper_list));
                }
            }
            Operation::BorrowGlobal(local_index_dst, _, _, wrapper_list) => {
                update_write(&alloca_needed, local_index_dst);
                update_wrappers(&alloca_needed, wrapper_list);
            },
            Operation::PointerOffset(local_index_dst, _flag, local_index_base, wrapper_list) => {
                update_write(&alloca_needed, local_index_dst);
                update_use(&alloca_needed, local_index_base);
                update_wrappers(&alloca_needed, wrapper_list);
            }
            }
        }
    }

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
