use crate::helpers::BitSet;

pub fn check_ssa(ir: &super::Expr)
{
    struct V {
        written: BitSet,
    }
    impl super::visit::Visitor for V {
        fn writes_slot(&mut self, addr: super::visit::Addr, local_index: &super::LocalIndex) {
            if self.written.set(local_index.0) {
                panic!("Multi-write of {:?} at {}", local_index, addr)
            }
        }
    }
    let mut v = V {
        written: BitSet::new(ir.locals.len()),
    };
    super::visit::visit_expr(&mut v, ir);
}