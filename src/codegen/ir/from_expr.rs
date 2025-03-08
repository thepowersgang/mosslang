use crate::INDENT;

struct LoopEntry {
    head: super::BlockIndex,
    exit: super::BlockIndex,
    break_slot: super::LocalIndex,
}

#[derive(Default)]
struct Output {
    cur_block: usize,
    cur_block_stmts: Vec<super::Operation>,

    local_types: Vec<crate::ast::Type>,
    blocks: Vec<Option<super::Block>>,
}

pub struct Visitor<'a> {
    parent: &'a mut super::super::State,
    loop_stack: Vec<LoopEntry>,
    output: Output,
}
impl<'a> Visitor<'a> {
    pub fn new(parent: &'a mut super::super::State, locals: &[crate::ast::Type]) -> Self {
        let mut rv = Visitor {
            parent,
            loop_stack: Vec::new(),
            output: Default::default(),
        };
        rv.output.local_types = locals.iter().cloned().collect();
        rv.output.blocks.push(None);    // We start with block #0 active
        rv
    }
    pub fn visit_expr(&mut self, expr: &crate::ast::expr::Expr) -> super::Value {
        let _i = INDENT.inc_f("visit_expr", format_args!("{:?}", &expr.kind));
        use super::{Value,Terminator,Operation};
        use crate::ast::expr::ExprKind;
        match &expr.kind {
        ExprKind::Block(block) => {
            self.visit_expr_block(block)
        },

        ExprKind::LiteralString(data) => super::Value::StringLiteral(data.clone()),
        ExprKind::LiteralInteger(v, _int_lit_class) => super::Value::IntegerLiteral(*v),

        ExprKind::Return(expr) => {
            let e = if let Some(expr) = expr { self.visit_expr(expr) } else { Value::ImplicitUnit };
            self.output.end_block(Terminator::Return(e));

            let b = self.output.new_block();
            self.output.start_block(b);
            Value::Unreachable
        },
        ExprKind::Continue => {
            let Some(l) = self.loop_stack.last() else { panic!("`continue` not in a loop") };
            self.output.end_block(Terminator::Goto(l.head));

            let b = self.output.new_block();
            self.output.start_block(b);
            Value::Unreachable
        }
        ExprKind::Break(expr) => {
            let e = if let Some(expr) = expr { self.visit_expr(expr) } else { Value::ImplicitUnit };
            let Some(l) = self.loop_stack.last() else { panic!("`break` not in a loop") };
            self.output.push_stmt(Operation::AssignLocal(l.break_slot, Default::default(), e));
            self.output.end_block(Terminator::Goto(l.exit));

            let b = self.output.new_block();
            self.output.start_block(b);
            Value::Unreachable
        }
        ExprKind::Assign { slot, op, value } => {
            let v_slot = self.visit_expr(slot);
            let v_value = self.visit_expr(value);
            match op {
            Some(_) => todo!("op-assign"),
            None => match v_slot {
                Value::Unreachable => Value::Unreachable,
                Value::StringLiteral(items) => todo!(),
                Value::IntegerLiteral(_) => todo!(),
                Value::ImplicitUnit => todo!(),
                Value::Local(local_index, wrapper_list) => {
                    self.output.push_stmt(Operation::AssignLocal(local_index, wrapper_list, v_value));
                    Value::ImplicitUnit
                    },
                Value::Named(absolute_path, wrapper_list) => {
                    //self.output.push_stmt(Operation::AssignNamed(local_index, wrapper_list, v_value));
                    //Value::ImplicitUnit
                    todo!("Assign to a named deref?")
                    },
                }
            }
        },
        ExprKind::NamedValue(path, binding) => {
            let Some(b) = binding else { panic!("Unresolved ExprKind::NamedValue {:?} @ {:p}", path, expr) };
            use crate::ast::path::ValueBinding;
            match b {
            ValueBinding::Local(i) => Value::Local(super::LocalIndex(*i as _), Default::default()),
            ValueBinding::Function(absolute_path) => todo!(),
            ValueBinding::Static(ap) => Value::Named(ap.clone(), Default::default()),
            ValueBinding::Constant(absolute_path) => todo!(),
            ValueBinding::StructValue(absolute_path) => todo!(),
            ValueBinding::EnumVariant(absolute_path, idx) => {
                // HACK: Assume that the variant isn't a data-holding variant
                Value::IntegerLiteral(*idx as _)
                },
            }
        },
        ExprKind::CallPath(path, binding, exprs) => {
            let rv = self.output.allocate_slot(&expr.data_ty);
            let a: Vec<_> = exprs.iter().map(|e| self.visit_expr(e)).collect();
            let Some(b) = binding else { panic!("Unresolved ExprKind::Callpath {:?}", path) };
            use crate::ast::path::ValueBinding;
            match b {
            ValueBinding::Local(i) => todo!(),
            ValueBinding::Function(absolute_path) => {
                let next_block = self.output.new_block();
                self.output.end_block(Terminator::CallPath(rv, next_block, absolute_path.clone(), a));
                self.output.start_block(next_block);
            },
            ValueBinding::Static(ap) => todo!(),
            ValueBinding::Constant(absolute_path) => todo!(),
            ValueBinding::StructValue(absolute_path) => todo!(),
            ValueBinding::EnumVariant(absolute_path, idx) => todo!(),
            }
            Value::Local(rv, Default::default())
        },
        ExprKind::Tuple(exprs) => todo!(),
        ExprKind::FieldNamed(expr, ident) => {
            let v = self.visit_expr(expr);
            todo!("Get field index for {} from {:?}", ident, expr.data_ty);
        },
        ExprKind::FieldIndex(expr, _) => todo!(),
        ExprKind::Index(expr_v, expr_i) => {    
            let v = self.visit_expr(expr_v);
            let i = self.visit_expr(expr_i);
            let w = match i {
                Value::Unreachable => None,
                Value::ImplicitUnit => panic!("Type error: Indexing by unit"),
                Value::StringLiteral(_) => panic!("Type error: Indexing by String"),
                Value::IntegerLiteral(i) => Some(super::Wrapper::Field(i as usize)),
                Value::Local(idx, wrappers) if wrappers.is_empty() =>
                    Some(super::Wrapper::IndexBySlot(idx)),
                Value::Local(..) | Value::Named(..) => {
                    let s = self.output.allocate_slot(&expr_i.data_ty);
                    self.output.push_stmt(Operation::AssignLocal(s, Default::default(), i));
                    Some(super::Wrapper::IndexBySlot(s))
                }
                };
            match (v,w) {
            (Value::Unreachable,_) => Value::Unreachable,
            (_,None) => Value::Unreachable,
            (Value::ImplicitUnit     ,_) => panic!("Type error: Indexing a unit"),
            (Value::IntegerLiteral(_),_) => panic!("Type error: Indexing an integer"),

            (Value::StringLiteral(_),_) => todo!("Indexing a string?"),
            (Value::Local(local_index, mut wrapper_list),Some(w)) => {
                wrapper_list.push(w);
                Value::Local(local_index, wrapper_list)
                },
            (Value::Named(absolute_path, mut wrapper_list), Some(w)) => {
                wrapper_list.push(w);
                Value::Named(absolute_path, wrapper_list)
                },
            }
        },
        ExprKind::Addr(_, expr) => todo!(),
        ExprKind::Deref(expr) => {
            let v = self.visit_expr(expr);
            match v {
            Value::Unreachable => Value::Unreachable,
            Value::StringLiteral(_) => panic!("Deref of string literal?"),
            Value::IntegerLiteral(_) => panic!("Type error: Deref of integer"),
            Value::ImplicitUnit => panic!("Type error: Deref of unit"),
            Value::Local(local_index, wrapper_list) =>
                Value::Local(local_index, wrapper_list.add(super::Wrapper::Deref)),
            Value::Named(absolute_path, wrapper_list) =>
            Value::Named(absolute_path, wrapper_list.add(super::Wrapper::Deref)),
            }
        },
        ExprKind::Cast(expr, _) => todo!(),
        ExprKind::UniOp(uni_op_ty, expr) => {
            let rv = self.output.allocate_slot(&expr.data_ty);
            let v = self.visit_expr(expr);
            let op = match uni_op_ty {
                crate::ast::expr::UniOpTy::Invert => super::UniOp::Not,
                crate::ast::expr::UniOpTy::Negate => super::UniOp::Neg,
                };
            self.output.push_stmt(Operation::UniOp(rv, op, v));
            Value::Local(rv, Default::default())
        },
        ExprKind::BinOp(bin_op_ty, expr_l, expr_r) => {
            let rv = self.output.allocate_slot(&expr.data_ty);
            let v_l = self.visit_expr(expr_l);
            let v_r = self.visit_expr(expr_r);

            use crate::ast::expr::Expr;
            fn binop(this: &mut Visitor, rv: super::LocalIndex, expr_l: &Expr, op: super::BinOp, expr_r: &Expr) {
                let v_l = this.visit_expr(expr_l);
                let v_r = this.visit_expr(expr_r);
                this.output.push_stmt(Operation::BinOp(rv, v_l, super::BinOp::Sub, v_r))
            }
            fn cmp(this: &mut Visitor, rv: super::LocalIndex, expr_l: &Expr, op: super::CmpOp, expr_r: &Expr) {
                let v_l = this.visit_expr(expr_l);
                let v_r = this.visit_expr(expr_r);
                let bb_true  = this.output.new_block();
                let bb_false = this.output.new_block();
                let bb_exit  = this.output.new_block();
                this.output.end_block(Terminator::Compare(v_l, op, v_r, bb_true, bb_false));
            
                this.output.start_block(bb_true);
                this.output.push_stmt(Operation::AssignLocal(rv, Default::default(), Value::IntegerLiteral(1)));
                this.output.end_block(Terminator::Goto(bb_exit));
            
                this.output.start_block(bb_false);
                this.output.push_stmt(Operation::AssignLocal(rv, Default::default(), Value::IntegerLiteral(0)));
                this.output.end_block(Terminator::Goto(bb_exit));

                this.output.start_block(bb_exit);
            }

            use crate::ast::expr::BinOpTy;
            match bin_op_ty {
            BinOpTy::Add => binop(self, rv, expr_l, super::BinOp::Add, expr_r),
            BinOpTy::Sub => binop(self, rv, expr_l, super::BinOp::Sub, expr_r),
            BinOpTy::Mul => binop(self, rv, expr_l, super::BinOp::Mul, expr_r),
            BinOpTy::Div => binop(self, rv, expr_l, super::BinOp::Div, expr_r),
            BinOpTy::Rem => binop(self, rv, expr_l, super::BinOp::Rem, expr_r),
            BinOpTy::BitAnd => todo!(),
            BinOpTy::BitOr  => todo!(),
            BinOpTy::BitXor => todo!(),
            BinOpTy::Shl => todo!(),
            BinOpTy::Shr => todo!(),
            
            BinOpTy::Equals    => cmp(self, rv, expr_l, super::CmpOp::Eq, expr_r),
            BinOpTy::NotEquals => cmp(self, rv, expr_l, super::CmpOp::Ne, expr_r),
            BinOpTy::Lt        => cmp(self, rv, expr_l, super::CmpOp::Lt, expr_r),
            BinOpTy::LtEquals  => cmp(self, rv, expr_l, super::CmpOp::Le, expr_r),
            BinOpTy::Gt        => cmp(self, rv, expr_l, super::CmpOp::Gt, expr_r),
            BinOpTy::GtEquals  => cmp(self, rv, expr_l, super::CmpOp::Ge, expr_r),

            BinOpTy::BoolAnd => todo!(),
            BinOpTy::BoolOr  => {
                let bb_true = self.output.new_block();
                let bb_alt = self.output.new_block();
                let bb_false = self.output.new_block();
                let bb_exit = self.output.new_block();
                self.apply_if(expr_l, bb_true, bb_alt);
                
                self.output.start_block(bb_alt);
                self.apply_if(expr_r, bb_true, bb_false);
                
                self.output.start_block(bb_true);
                self.output.push_stmt(Operation::AssignLocal(rv, Default::default(), Value::IntegerLiteral(1)));
                self.output.end_block(Terminator::Goto(bb_exit));
                
                self.output.start_block(bb_false);
                self.output.push_stmt(Operation::AssignLocal(rv, Default::default(), Value::IntegerLiteral(0)));
                self.output.end_block(Terminator::Goto(bb_exit));

                self.output.start_block(bb_exit);
            },
            }
            Value::Local( rv, Default::default() )
        },
        ExprKind::CallValue(expr, exprs) => todo!(),

        ExprKind::Loop { body } => {
            let bb_head = self.output.new_block(); // No args?
            let bb_exit = self.output.new_block(); // Args: break value
            let break_slot = self.output.allocate_slot(&expr.data_ty);
            self.loop_stack.push(LoopEntry {
                head: bb_head,
                exit: bb_exit,
                break_slot,
            });
            self.output.end_block(Terminator::Goto(bb_head));
            self.output.start_block(bb_head);
            self.visit_expr_block(body);
            self.output.end_block(Terminator::Goto(bb_head));
            self.output.start_block(bb_exit);
            Value::Local( self.loop_stack.pop().unwrap().break_slot, Default::default() )
        },
        ExprKind::WhileLoop { cond, body, else_block } => todo!("while loop"),
        ExprKind::ForLoop { pattern, start, end, body, else_block } => {
            let slot_it_value = self.output.allocate_slot(&start.data_ty);
            let start_value = self.visit_expr(start);
            let end_value = self.visit_expr(end);
            let bb_head = self.output.new_block(); // No args?
            let bb_inc = self.output.new_block(); // ?
            let bb_body = self.output.new_block(); // ?
            let bb_else = self.output.new_block(); // No args?
            let bb_exit = self.output.new_block(); // Args: break value
            let break_slot = self.output.allocate_slot(&expr.data_ty);
            
            self.loop_stack.push(LoopEntry {
                head: bb_inc,
                exit: bb_exit,
                break_slot,
            });
            
            self.output.push_stmt(Operation::AssignLocal(slot_it_value, Default::default(), start_value));
            self.output.end_block(Terminator::Goto(bb_head));

            self.output.start_block(bb_head);
            self.output.end_block(Terminator::Compare(Value::Local(slot_it_value, Default::default() ), super::CmpOp::Eq, end_value,  bb_else, bb_body));

            self.output.start_block(bb_body);
            self.destructure_pattern(pattern, Value::Local(slot_it_value, Default::default() ));
            self.visit_expr_block(body);
            self.output.end_block(Terminator::Goto(bb_inc));

            self.output.start_block(bb_inc);
            self.output.push_stmt(Operation::BinOp(slot_it_value, Value::Local(slot_it_value, Default::default() ), super::BinOp::Add, Value::IntegerLiteral(1)));
            
            self.output.end_block(Terminator::Goto(bb_head));
            let break_slot = self.loop_stack.pop().unwrap().break_slot;

            self.output.start_block(bb_else);
            if let Some(else_block) = else_block {
                self.visit_expr_block(else_block);
            }
            else {
                self.output.push_stmt(Operation::AssignLocal(break_slot, Default::default(), Value::ImplicitUnit));
            }
            self.output.end_block(Terminator::Goto(bb_exit));

            self.output.start_block(bb_exit);
            Value::Local( break_slot, Default::default() )
        },
        ExprKind::IfChain { branches, else_block } => {
            let res_slot = self.output.allocate_slot(&expr.data_ty);
            let bb_exit = self.output.new_block();
            for b in branches {
                let bb_body = self.output.new_block();
                let bb_next = self.output.new_block();
                self.apply_if(&b.cond, bb_body, bb_next);

                self.output.start_block(bb_body);
                let v = self.visit_expr_block(&b.body);
                self.output.push_stmt(Operation::AssignLocal(res_slot, Default::default(), v));
                self.output.end_block(Terminator::Goto(bb_exit));

                self.output.start_block(bb_next);
            }
            let ev = if let Some(else_block) = else_block {
                self.visit_expr_block(else_block)
            }
            else {
                Value::ImplicitUnit
            };
            self.output.push_stmt(Operation::AssignLocal(res_slot, Default::default(), ev));
            self.output.end_block(Terminator::Goto(bb_exit));

            self.output.start_block(bb_exit);
            Value::Local( res_slot, Default::default() )
        },
        ExprKind::Match { value, branches } => todo!("match"),
        }
    }
    fn visit_expr_block(&mut self, block: &crate::ast::expr::Block) -> super::Value {
        for stmt in &block.statements {
            match stmt {
            crate::ast::expr::Statement::Expr(expr) => {
                self.visit_expr(expr);
                },
            crate::ast::expr::Statement::Let(pattern, _, expr) => {
                if let Some(expr) = expr {
                    let value = self.visit_expr(expr);
                    self.destructure_pattern(pattern, value);
                }
            },
            }
        }
        if let Some(expr) = &block.result {
            self.visit_expr(expr)
        }
        else {
            super::Value::ImplicitUnit
        }
    }

    fn destructure_pattern(&mut self, pattern: &crate::ast::Pattern, value: super::Value) {
        use crate::ast::PatternTy;
        match &pattern.ty {
        PatternTy::Any => {},
        PatternTy::MaybeBind(_) => unreachable!("Should have been resolved"),
        PatternTy::NamedValue(_) => {},
        PatternTy::Tuple(patterns) => {
            for (i,sp) in patterns.iter().enumerate() {
                self.destructure_pattern(sp, value.field(i));
            }
            },
        }
        if pattern.bindings.len() > 1 {
            todo!("Multiple bindings?");
        }
        //for b in &pattern.bindings {
        if let Some(b) = pattern.bindings.first() {
            // Create variable? Should already be created, just needs to be assigned.
            let i = b.index.expect("pattern binding not bound");
            self.output.push_stmt(super::Operation::AssignLocal(super::LocalIndex(i as _), Default::default(), value));
        }
    }

    fn apply_if(&mut self, expr: &crate::ast::expr::Expr, bb_true: super::BlockIndex, bb_false: super::BlockIndex) {
        // HACK: Special-case comparison ops
        if let crate::ast::expr::ExprKind::BinOp(op_ty, e_l, e_r) = &expr.kind {
            use crate::ast::expr::BinOpTy;
            let op = match op_ty {
                BinOpTy::Equals    => Some(super::CmpOp::Eq),
                BinOpTy::NotEquals => Some(super::CmpOp::Ne),
                BinOpTy::Lt       => Some(super::CmpOp::Lt),
                BinOpTy::LtEquals => Some(super::CmpOp::Le),
                BinOpTy::Gt       => Some(super::CmpOp::Gt),
                BinOpTy::GtEquals => Some(super::CmpOp::Ge),
                //BinOpTy::BoolAnd => todo!(),
                //BinOpTy::BoolOr => todo!(),
                _ => None,
                };
            if let Some(op) = op {
                let v_l = self.visit_expr(e_l);
                let v_r = self.visit_expr(e_r);
                self.output.end_block(super::Terminator::Compare(v_l, op, v_r,  bb_true, bb_false));
                return ;
            }
        }
        let cond_v = self.visit_expr(expr);
        self.output.end_block(super::Terminator::Compare(cond_v, super::CmpOp::Ne, super::Value::IntegerLiteral(0),  bb_true, bb_false));
    }
}
impl Output {
    fn new_block(&mut self) -> super::BlockIndex {
        let i = self.blocks.len();
        self.blocks.push(None);
        super::BlockIndex(i)
    }
    fn allocate_slot(&mut self, v: &crate::ast::Type) -> super::LocalIndex {
        let i = self.local_types.len();
        self.local_types.push(v.clone());
        super::LocalIndex(i)
    }

    // TODO: Block arguments?
    fn start_block(&mut self, index: super::BlockIndex) {
        println!("{INDENT}start_block: {index:?}");
        assert!(self.cur_block == usize::MAX);
        assert!(index.0 < self.blocks.len());
        assert!(self.blocks[index.0].is_none(), "Block #{} already filled - {:?}", index.0, self.blocks[index.0].as_ref().unwrap().statements);
        assert!(self.cur_block_stmts.is_empty());
        self.cur_block = index.0;
    }
    fn push_stmt(&mut self, stmt: super::Operation) {
        println!("{INDENT}push_stmt: {stmt:?}");
        assert!(self.cur_block != usize::MAX, "Pushing with no open block");
        self.cur_block_stmts.push(stmt);
    }
    fn end_block(&mut self, terminator: super::Terminator) {
        println!("{INDENT}end_block: {terminator:?}");
        assert!(self.cur_block != usize::MAX);
        self.blocks[self.cur_block] = Some(super::Block {
            statements: ::std::mem::take(&mut self.cur_block_stmts),
            terminator,
        });
        self.cur_block = usize::MAX;
    }
}