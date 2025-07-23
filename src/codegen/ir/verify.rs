use crate::helpers::BitSet;
use crate::INDENT;

pub fn check(ir: &super::Expr, arg_count: usize)
{
    let mut failures = FailureList::new();
    write_before_use(&mut failures, ir, arg_count);
    failures.check(ir);
}
pub fn check_ssa(ir: &super::Expr, arg_count: usize)
{
    let mut failures = FailureList::new();
    write_once(&mut failures, ir);
    write_before_use(&mut failures, ir, arg_count);
    failures.check(ir);
}

struct FailureList {
    items: Vec<(super::visit::Addr, String)>,
}
impl FailureList {
    fn new() -> Self {
        FailureList { items: Default::default() }
    }
    fn push(&mut self, addr: super::visit::Addr, msg: String) {
        println!(">>push() @{}: {}", addr, msg);
        self.items.push((addr, msg));
    }
    fn check(mut self, ir: &super::Expr) {
        if !self.items.is_empty() {
            eprintln!("Validation failures:");
            self.items.sort();
            self.items.dedup();
            for (addr,msg) in &self.items {
                eprintln!("@{}: {}", addr, msg)
            }
            if false {
                let _ = super::dump::dump(&mut ::std::io::stdout().lock(), ir);
            }
            panic!("Validation failures found");
        }
    }
}

fn write_once(failures: &mut FailureList, ir: &super::Expr) {
    struct V<'a> {
        failures: &'a mut FailureList,
        written: BitSet,
    }
    impl super::visit::Visitor for V<'_> {
        fn writes_slot(&mut self, addr: super::visit::Addr, local_index: &super::LocalIndex) {
            if self.written.set(local_index.0) {
                self.failures.push(addr, format!("Multi-write of {:?}", local_index));
            }
        }
    }
    let mut v = V {
        failures,
        written: BitSet::new(ir.locals.len()),
    };
    super::visit::visit_expr(&mut v, ir);
}

fn write_before_use(failures: &mut FailureList, ir: &super::Expr, arg_count: usize) {
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
                            println!("{INDENT}verify::#write_before_use::maybe_push: bb{} _{} unset", idx, i);
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
        struct V<'a> {
            failures: &'a mut FailureList,
            states: BitSet,
        }
        impl super::visit::Visitor for V<'_> {
            fn reads_slot(&mut self, addr: super::visit::Addr, local_index: &super::LocalIndex) {
                if !self.states.is_set(local_index.0) {
                    self.failures.push(addr, format!("Use of unset local {:?}", local_index));
                }
            }
            fn writes_slot(&mut self, _addr: super::visit::Addr, local_index: &super::LocalIndex) {
                self.states.set(local_index.0);
                // Note: Can't check multiple-writes here, as this function loops several times
            }
        }
        let mut v = V {
            failures,
            states: block_states[idx].clone().unwrap(),
        };
        println!("{INDENT}verify::#write_before_use: bb{}", idx);
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