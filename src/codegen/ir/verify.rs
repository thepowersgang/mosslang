use crate::helpers::BitSet;
use crate::INDENT;

pub fn check(ir: &super::Expr, arg_count: usize)
{
    write_before_use(ir, arg_count);
}
pub fn check_ssa(ir: &super::Expr, arg_count: usize)
{
    write_once(ir);
    write_before_use(ir, arg_count);
}

fn write_once(ir: &super::Expr) {
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

fn write_before_use(ir: &super::Expr, arg_count: usize) {
    let mut block_states: Vec<_> = ir.blocks.iter().map(|_| None).collect();
    let mut stack = Vec::with_capacity(32);
    block_states[0] = Some({
        let mut v = crate::helpers::BitSet::new(ir.locals.len());
        for i in 0 .. arg_count {
            v.set(i);
        }
        v
        });
    stack.push(0);
    fn maybe_push(stack: &mut Vec<usize>, block_states: &mut [Option<BitSet>], idx: usize, states: &BitSet) {
        let changed = match block_states[idx] {
            ref mut d @ None => {
                *d = Some(states.clone());
                true
            },
            Some(ref mut s) => {
                let mut change = false;
                for i in 0 .. states.cap() {
                    // If the value isn't set in the incoming, then clear it in `s` and try again
                    if !states.is_set(i) {
                        if s.clear(i) {
                            println!("{INDENT}bb{} _{} unset", idx, i);
                            change = true;
                        }
                    }
                }
                change
            },
            };
        if changed {
            stack.push(idx);
        }
    }
    while let Some(idx) = stack.pop() {
        struct V {
            states: BitSet,
        }
        impl super::visit::Visitor for V {
            fn reads_slot(&mut self, addr: super::visit::Addr, local_index: &super::LocalIndex) {
                if !self.states.is_set(local_index.0) {
                    panic!("Use of unset local {:?} at {}", local_index, addr)
                }
            }
            fn writes_slot(&mut self, _addr: super::visit::Addr, local_index: &super::LocalIndex) {
                self.states.set(local_index.0);
                // Note: Can't check multiple-writes here, as this function loops several times
            }
        }
        let mut v = V {
            states: block_states[idx].clone().unwrap(),
        };
        println!("{INDENT}bb{}", idx);
        let b = &ir.blocks[idx];
        for a in b.args.iter() {
            v.states.set(a.0);
        }
        super::visit::visit_block(&mut v, super::BlockIndex(idx), b);

        use crate::codegen::ir::Terminator;
        match &b.terminator {
        Terminator::Unreachable|Terminator::Return(_) => {},
        Terminator::Goto(tgt)
        |Terminator::CallPath { tgt, .. }
        |Terminator::CallValue { tgt, .. } => {
            maybe_push(&mut stack, &mut block_states, tgt.index, &v.states);
        },
        Terminator::Compare { if_true, if_false, .. }
        |Terminator::MatchEnum { if_true, if_false, .. } => {
            maybe_push(&mut stack, &mut block_states, if_true.index, &v.states);
            maybe_push(&mut stack, &mut block_states, if_false.index, &v.states);
        },
        }
    }
}