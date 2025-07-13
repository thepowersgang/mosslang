//! Convert an IR expression into SSA form
//! 
//! Same underlying representation, but uses `Operation::CreateSlot`
use crate::INDENT;
use crate::helpers::BitSet;
use super::{Operation,Terminator};
use super::visit::VisitorMut;

pub fn from_expr(mut ir: super::Expr) -> super::Expr
{
    let _i = INDENT.inc("ssa_ify");
    // Need to handle the following:
    // - Create `alloca`s if a variable is borrowed (as that requires taking a pointer to the variable)
    // - Deconstruct multi-write values into separate SSA variables, joined by block parameters
    let borrowed;
    let twice_written;
    {
        struct VisitorEnum {
            borrowed: BitSet,
            borrowed_mut: BitSet,
            written: BitSet,
            twice_written: BitSet,
        }
        impl super::visit::Visitor for VisitorEnum {
            fn writes_slot(&mut self, addr: super::visit::Addr, local_index: &super::LocalIndex) {
                if self.written.set(local_index.0) {
                    if !self.twice_written.set(local_index.0) {
                        println!("{INDENT}{local_index:?}: Write twice @ {addr}",);
                    }
                }
            }
            fn operation(&mut self, addr: super::visit::Addr, op: &super::Operation) {
                match op {
                Operation::AssignDeref(_, _) => {},
                Operation::BorrowLocal(local_index_dst, is_mut, local_index_src, _wrappers) => {
                    if *is_mut && !self.borrowed_mut.set(local_index_src.0) {
                        println!("{INDENT}{local_index_src:?}: Borrowed mut @ {addr}",);
                    }
                    if !self.borrowed.set(local_index_src.0) {
                        println!("{INDENT}{local_index_src:?}: Borrowed @ {addr}",);
                    }
                    self.writes_slot(addr, local_index_dst);
                },
                _ => super::visit::visit_operation(self, addr, op),
                }
            }
        }
        let mut v = VisitorEnum {
            borrowed: BitSet::new(ir.locals.len()),
            borrowed_mut: BitSet::new(ir.locals.len()),
            written: BitSet::new(ir.locals.len()),
            twice_written: BitSet::new(ir.locals.len()),
        };
        super::visit::visit_expr(&mut v, &ir);
        VisitorEnum { borrowed, borrowed_mut: _, written: _, twice_written } = v;
    }
    
    // For all of the multi-write values (and not borrowed), if they can be trivially turned into block params instead of making `alloca`s
    // - If all writes jump to the same block
    // - TODO: More complex versions - generate paths between writes and reads (de-duplicated to shortest).
    //   > Find common point and inject block params there
    for slot in 0 .. ir.locals.len() {
        if twice_written.is_set(slot) && !borrowed.is_set(slot) {
            let _i = INDENT.inc_f("ssa_ify: write", format_args!("_{slot}",));
            
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

            #[derive(PartialEq, Eq, PartialOrd, Ord)]
            struct RemapKey {
                addr: super::visit::Addr,
                after_read: bool,
            }
            impl ::std::fmt::Debug for RemapKey {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    write!(f, "{}{}", self.addr, if self.after_read { "w" } else { "" })
                }
            }
            let mut remap_table = ::std::collections::BTreeMap::<RemapKey,super::LocalIndex>::new();

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
                    false
                    //|| (a.stmt_idx == 0 && common_blocks.is_set(a.block_idx.0))   // This would be an optimisation, but can't capture
                    //|| rs.reads.contains(&a)  // Can't stop on a read, as there may be other reads through common blocks after it
                    || (a != *w && rs.writes.contains(&a))  // But can (and should) stop on a write
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
                        // NOTE: We only care about blocks entered, hence the `.skip(1)`
                        for path in other_write_routes[..path_count].iter() {
                            for bb_idx in route.blocks().skip(1) {
                                if path.blocks().skip(1).any(|v| v == bb_idx) {
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
                    remap_table.insert(RemapKey { addr: super::visit::Addr { block_idx: super::BlockIndex(block_idx), stmt_idx: 0}, after_read: false }, v);
                }
            }
            for &w in &rs.writes {
                let ty = ir.locals[slot].clone();
                remap_table.insert(RemapKey { addr: w, after_read: true }, super::LocalIndex(ir.locals.len()));
                ir.locals.push(ty);
            }
            println!("{INDENT}remap_table = {remap_table:?}",);

            // Propagate entries in the remap table
            {
                let mut exit_table: Vec<_> = (0 .. ir.blocks.len()).map(|_| None).collect();
                for (a,v) in remap_table.iter() {
                    // Since `remap_table` above is a BTreeMap, it's sorted - so later remaps come first. This means that if we overwrite the `exit_table` entry it'll be correct
                    exit_table[a.addr.block_idx.0] = Some(*v);
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
                            println!("{INDENT}bb{block_idx} -> bb{b}: +arg {local:?}", b=tgt.index);
                            tgt.args.push(local);
                        }
                        else {
                            remap_table.insert(RemapKey { addr: super::visit::Addr { block_idx: super::BlockIndex(tgt.index), stmt_idx: 0}, after_read: false }, local);
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
                remap_table: ::std::collections::BTreeMap<RemapKey,super::LocalIndex>,
            }
            impl VisitorMut for V {
                fn writes_slot(&mut self, addr: super::visit::Addr, local_index: &mut super::LocalIndex) {
                    if local_index.0 == self.slot {
                        let n = self.remap_table[&RemapKey { addr, after_read: true }];
                        //let n = *self.remap_table.range(..=addr).last().unwrap().1
                        println!("{INDENT}writes_slot: @{addr} {local_index:?} -> {n:?}");
                        *local_index = n;
                    }   
                }
                fn reads_slot(&mut self, addr: super::visit::Addr, local_index: &mut super::LocalIndex) {
                    if local_index.0 == self.slot {
                        let n = *self.remap_table.range(..=RemapKey { addr, after_read: false }).last().unwrap().1;
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
        for (stmt_idx,stmt) in block.statements.iter_mut().enumerate() {
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
        {
            let mut v = V { alloca_needed: &alloca_needed };
            let addr = super::visit::Addr { block_idx: super::BlockIndex(block_idx), stmt_idx: !0 };
            super::visit::visit_terminator_mut(&mut v, addr, &mut block.terminator);
        }
    }
    new_ops.append(&mut ir.blocks[0].statements);
    ir.blocks[0].statements = new_ops;
    
    // Update local types for alloca'd locals
    for slot in 0 .. ir.locals.len() {
        if alloca_needed.is_set(slot) {
            let ty = &mut ir.locals[slot];
            let kind = ::std::mem::replace(&mut ty.kind, crate::ast::ty::TypeKind::Void);
            ty.kind = crate::ast::ty::TypeKind::Pointer {
                is_const: false,    // TODO: Get the mutability of borrows?
                inner: Box::new(crate::ast::Type { span: ty.span.clone(), kind }),
            };
        }
    }

    ir
}