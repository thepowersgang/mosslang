//! Convert an IR expression into SSA form
//! 
//! Same underlying representation, but uses `Operation::CreateSlot`

use super::Operation;

pub fn from_expr(ir: super::Expr) -> super::Expr
{
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

            
            // Enumerate all paths from each write point
            // - Stop enumerating when another write position is seen?
            for w in rs.writes.iter() {
                // Check if there's another write in the same block, if there is then allocate a new local for the first one
                super::visit::enumerate_paths_from(&ir, *w, |route| {
                    // Trim at any other write
                });
            }
        }
    }

    todo!()
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
