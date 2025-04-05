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

pub struct Visitor<'a,'b> {
    parent: &'a mut super::super::State<'b>,
    loop_stack: Vec<LoopEntry>,
    output: Output,
}
impl<'a,'b> Visitor<'a,'b> {
    pub fn new(parent: &'a mut super::super::State<'b>, locals: &[crate::ast::Type]) -> Self {
        let mut rv = Visitor {
            parent,
            loop_stack: Vec::new(),
            output: Default::default(),
        };
        rv.output.local_types = locals.iter().cloned().collect();
        rv.output.blocks.push(None);    // We start with block #0 active
        rv
    }
    pub fn finish(mut self, tail_val: super::Value) -> super::Expr {
        match tail_val {
        super::Value::Unreachable => {},
        _ => self.output.end_block(super::Terminator::Return(tail_val)),
        }
        super::Expr {
            locals: self.output.local_types,
            blocks: self.output.blocks.into_iter().map(|b| b.unwrap()).collect(),
        }
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
            let v_value = match op {
                None => v_value,
                Some(op) => {
                    use crate::ast::expr::AssignOp;
                    let rv = self.output.allocate_slot(&slot.data_ty);
                    let op = match op {
                        AssignOp::Add => super::BinOp::Add,
                        AssignOp::Sub => super::BinOp::Sub,
                        _ => todo!("op-assign {:?}", op),
                        };
                    self.output.push_stmt(Operation::BinOp(rv, v_slot.clone(), op, v_value));
                    Value::Local(rv, Default::default())
                },
                };
            match v_slot {
            Value::Unreachable => Value::Unreachable,
            Value::StringLiteral(_) => panic!("Type error: Assigning to string literal"),
            Value::IntegerLiteral(_) => panic!("Type error: Assigning to integer literal"),
            Value::ImplicitUnit => panic!("Type error: Assigning to unit"),
            Value::Local(local_index, wrapper_list) => {
                self.output.push_stmt(Operation::AssignLocal(local_index, wrapper_list, v_value));
                Value::ImplicitUnit
                },
            Value::Named(absolute_path, wrapper_list) => {
                if wrapper_list.is_empty() {
                    self.output.push_stmt(Operation::AssignNamed(absolute_path, v_value));
                }
                else {
                    todo!("Assign to a named deref?")
                }
                Value::ImplicitUnit
                },
            Value::Deref { ptr, wrappers } => {
                todo!("Assign to deref")
                //self.output.push_stmt(Operation::AssignDeref(ptr, wrappers, v_value));
                //Value::ImplicitUnit
                },
            }
        },
        ExprKind::NamedValue(path, binding) => {
            let Some(b) = binding else { panic!("Unresolved ExprKind::NamedValue {:?} @ {:p}", path, expr) };
            use crate::ast::path::ValueBinding;
            match b {
            ValueBinding::Local(i) => Value::Local(super::LocalIndex(*i as _), Default::default()),
            ValueBinding::Function(absolute_path) => todo!("function pointer"),
            ValueBinding::DataEnumVariant(absolute_path, _) => todo!("function pointer"),
            ValueBinding::Static(ap) => Value::Named(ap.clone(), Default::default()),
            ValueBinding::Constant(absolute_path) => {
                self.visit_expr( &self.parent.constants.get(absolute_path).expect("Missing constant?").e )
            },
            ValueBinding::StructValue(absolute_path) => todo!("function pointer - struct"),
            ValueBinding::ValueEnumVariant(absolute_path, idx) => {
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
            ValueBinding::Static(absolute_path) => todo!(),
            ValueBinding::Constant(absolute_path) => todo!(),
            ValueBinding::StructValue(absolute_path) => todo!(),
            ValueBinding::DataEnumVariant(absolute_path, idx) => todo!("create data enum variant"),
            ValueBinding::ValueEnumVariant(absolute_path, _) => panic!("{}: Unexpected CallPath of value enum variant - {}", expr.span, absolute_path),
            }
            Value::Local(rv, Default::default())
        },
        ExprKind::Tuple(exprs) => {
            let rv = self.output.allocate_slot(&expr.data_ty);
            let a: Vec<_> = exprs.iter().map(|e| self.visit_expr(e)).collect();
            self.output.push_stmt(super::Operation::CreateComposite(rv, None, a));
            Value::Local(rv, Default::default())
        },
        ExprKind::FieldNamed(expr, ident) => {
            let v = self.visit_expr(expr);
            let data_ty = match &expr.data_ty.kind {
                crate::ast::ty::TypeKind::Named(_, Some(crate::ast::path::TypeBinding::Struct(p))) => p,
                //crate::ast::ty::TypeKind::Named(_, Some(crate::ast::path::TypeBinding::Union(p))) => p,
                _ => panic!("Unexpected type for named field - {}", expr.data_ty),
                };
            let Some(ty_fields) = self.parent.fields.get(data_ty) else { panic!("Type {} not in fields cache", data_ty) };
            let Some(&idx) = ty_fields.get(ident) else { panic!() };
            v.field(idx)
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
                Value::Local(..) | Value::Named(..) | Value::Deref { .. } => {
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
            (Value::Deref { ptr, mut wrappers }, Some(w)) => {
                wrappers.push(w);
                Value::Deref { ptr, wrappers }
                },
            }
        },
        ExprKind::Addr(is_mut, val_expr) => {
            let v = self.visit_expr(val_expr);
            match v {
            Value::Unreachable => Value::Unreachable,
            Value::ImplicitUnit => todo!("Borrow of an ImplicitUnit?"),
            Value::Local(local_index, wrapper_list) => {
                let rv = self.output.allocate_slot(&expr.data_ty);
                self.output.push_stmt(Operation::BorrowLocal(rv, *is_mut, local_index, wrapper_list));
                Value::Local(rv, Default::default())
            },
            Value::Named(absolute_path, wrapper_list) => todo!(),
            Value::Deref { .. } => todo!(),
            
            Value::StringLiteral(_) | Value::IntegerLiteral(_) => {
                let tmp = self.output.allocate_slot(&val_expr.data_ty);
                self.output.push_stmt(Operation::AssignLocal(tmp, Default::default(), v));
                let rv = self.output.allocate_slot(&expr.data_ty);
                self.output.push_stmt(Operation::BorrowLocal(rv, *is_mut, tmp, Default::default()));
                Value::Local(rv, Default::default())
            },
            }
        },
        ExprKind::Deref(val_expr) => {
            let v = self.visit_expr(val_expr);
            match v {
            Value::Unreachable => Value::Unreachable,
            Value::StringLiteral(_) => panic!("Deref of string literal?"),
            Value::IntegerLiteral(_) => panic!("Type error: Deref of integer"),
            Value::ImplicitUnit => panic!("Type error: Deref of unit"),
            Value::Local(local_index, wrapper_list) if wrapper_list.is_empty() => {
                Value::Deref { ptr: local_index, wrappers: Default::default() }
                }
            Value::Local(..) | Value::Named(..) | Value::Deref { .. } => {
                let tmp = self.output.allocate_slot(&val_expr.data_ty);
                self.output.push_stmt(Operation::AssignLocal(tmp, Default::default(), v));
                Value::Deref { ptr: tmp, wrappers: Default::default() }
                },
            }
        },
        ExprKind::Cast(val_expr, _) | ExprKind::Coerce(val_expr) => {
            let v = self.visit_expr(val_expr);
            use crate::ast::ty::TypeKind;
            match (&expr.data_ty.kind, &val_expr.data_ty.kind) {
            (ref t1, ref t2) if t1 == t2 => v,
            (TypeKind::Void, _) => Value::ImplicitUnit,
            (TypeKind::Pointer { .. },TypeKind::Pointer { .. }) => v,
            (TypeKind::Integer { .. },TypeKind::Integer { .. }) => v,   // TODO: Should this use an operation to truncate the value?
            (TypeKind::Integer { .. },TypeKind::Named(_, Some(crate::ast::path::TypeBinding::ValueEnum(_)))) => v,
            _ => todo!("{}: lower IR: convert {} to {} - {:?}", expr.span, val_expr.data_ty, expr.data_ty, v),
            }
        },
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

            use crate::ast::expr::Expr;
            fn binop(this: &mut Visitor, rv: super::LocalIndex, expr_l: &Expr, op: super::BinOp, expr_r: &Expr) {
                let v_l = this.visit_expr(expr_l);
                let v_r = this.visit_expr(expr_r);
                this.output.push_stmt(Operation::BinOp(rv, v_l, op, v_r))
            }
            fn bitshift(this: &mut Visitor, rv: super::LocalIndex, expr_l: &Expr, op: super::BitShift, expr_r: &Expr) {
                let v_l = this.visit_expr(expr_l);
                let v_r = this.visit_expr(expr_r);
                this.output.push_stmt(Operation::BitShift(rv, v_l, op, v_r))
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

            BinOpTy::BitAnd => binop(self, rv, expr_l, super::BinOp::BitAnd, expr_r),
            BinOpTy::BitOr  => binop(self, rv, expr_l, super::BinOp::BitOr , expr_r),
            BinOpTy::BitXor => binop(self, rv, expr_l, super::BinOp::BitXor, expr_r),

            BinOpTy::Shl => bitshift(self, rv, expr_l, super::BitShift::Left , expr_r),
            BinOpTy::Shr => bitshift(self, rv, expr_l, super::BitShift::Right, expr_r),
            
            BinOpTy::Equals    => cmp(self, rv, expr_l, super::CmpOp::Eq, expr_r),
            BinOpTy::NotEquals => cmp(self, rv, expr_l, super::CmpOp::Ne, expr_r),
            BinOpTy::Lt        => cmp(self, rv, expr_l, super::CmpOp::Lt, expr_r),
            BinOpTy::LtEquals  => cmp(self, rv, expr_l, super::CmpOp::Le, expr_r),
            BinOpTy::Gt        => cmp(self, rv, expr_l, super::CmpOp::Gt, expr_r),
            BinOpTy::GtEquals  => cmp(self, rv, expr_l, super::CmpOp::Ge, expr_r),

            BinOpTy::BoolOr|BinOpTy::BoolAnd => {
                let bb_true = self.output.new_block();
                let bb_alt = self.output.new_block();
                let bb_false = self.output.new_block();
                let bb_exit = self.output.new_block();

                if let BinOpTy::BoolAnd = *bin_op_ty {
                    self.apply_if(expr_l, bb_alt, bb_false);
                }
                else {
                    self.apply_if(expr_l, bb_true, bb_alt);
                }
                
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
        ExprKind::WhileLoop { cond, body, else_block } => {
            let break_slot = self.output.allocate_slot(&expr.data_ty);

            let bb_head = self.output.new_block(); // No args?
            let bb_body = self.output.new_block();
            let bb_else = self.output.new_block();
            let bb_exit = self.output.new_block();
            self.output.end_block(Terminator::Goto(bb_head));
            self.output.start_block(bb_head);
            self.apply_if(cond, bb_body, bb_else);
            
            self.loop_stack.push(LoopEntry {
                head: bb_head,
                exit: bb_exit,
                break_slot,
            });
            
            self.output.start_block(bb_body);
            self.visit_expr_block(body);
            self.output.end_block(Terminator::Goto(bb_head));

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
        ExprKind::Match { value, branches } => {

            // 1. Parse the patterns into a form that can be used to generate efficient code
            // - And do exhaustiveness checking

            let value = self.visit_expr(value);
            let res_slot = self.output.allocate_slot(&expr.data_ty);
            let bb_exit = self.output.new_block();
            for b in branches {
                let bb_body = self.output.new_block();
                let bb_next = self.output.new_block();
                self.match_pattern(&b.pat, value.clone(), bb_body, bb_next);

                self.output.start_block(bb_body);
                self.destructure_pattern(&b.pat, value.clone());
                let v = self.visit_expr(&b.val);
                self.output.push_stmt(Operation::AssignLocal(res_slot, Default::default(), v));
                self.output.end_block(Terminator::Goto(bb_exit));

                self.output.start_block(bb_next);
            }
            self.output.end_block(Terminator::Unreachable);

            self.output.start_block(bb_exit);
            Value::Local( res_slot, Default::default() )
        },
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

    fn match_pattern(&mut self, pattern: &crate::ast::Pattern, value: super::Value, bb_true: super::BlockIndex, bb_false: super::BlockIndex) {
        use crate::ast::PatternTy;
        match &pattern.ty {
        PatternTy::MaybeBind(_) => unreachable!("Should have been resolved"),
        PatternTy::Any => {},
        PatternTy::NamedValue(_, binding) => {
            let Some(binding) = binding else { unreachable!("Should have been resolved") };
            use crate::ast::path::ValueBinding;
            match binding {
            ValueBinding::Local(_) => todo!("Can't match to a local?"),
            ValueBinding::Function(_) => todo!("Can't match to a function."),
            ValueBinding::Static(_) => panic!("{span}: Attempting to match against a static", span=pattern.span),
            ValueBinding::StructValue(_) => {},
            ValueBinding::Constant(absolute_path) => {
                let cv = self.visit_expr( &self.parent.constants.get(absolute_path).expect("Missing constant?").e );
                self.output.end_block(super::Terminator::Compare(value, super::CmpOp::Eq, cv,  bb_true, bb_false));
                return ;
            },
            ValueBinding::ValueEnumVariant(_, var_idx) => {
                // Get the enum variant index
                //TODO: The below is only valid for non-data enums
                // Compare against this index
                self.output.end_block(super::Terminator::Compare(value, super::CmpOp::Eq, super::Value::IntegerLiteral(*var_idx as u128),  bb_true, bb_false));
                return ;
            },
            ValueBinding::DataEnumVariant(_, var_idx) => {
                todo!("{span}: Match data enum", span=pattern.span);
                }
            }
        },
        PatternTy::Tuple(patterns) => {
            for (i,sp) in patterns.iter().enumerate() {
                let bb_next = self.output.new_block();
                self.match_pattern(sp, value.field(i), bb_next, bb_false);
                self.output.start_block(bb_next);
            }
            },
        }
        self.output.end_block(super::Terminator::Goto(bb_true))

    }
    fn destructure_pattern(&mut self, pattern: &crate::ast::Pattern, value: super::Value) {
        use crate::ast::PatternTy;
        match &pattern.ty {
        PatternTy::Any => {},
        PatternTy::MaybeBind(_) => unreachable!("Should have been resolved"),
        PatternTy::NamedValue(_, _) => {},
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
    #[track_caller]
    fn end_block(&mut self, terminator: super::Terminator) {
        println!("{INDENT}end_block: {terminator:?}");
        assert!(self.cur_block != usize::MAX, "end_block with closed block");
        self.blocks[self.cur_block] = Some(super::Block {
            statements: ::std::mem::take(&mut self.cur_block_stmts),
            terminator,
        });
        self.cur_block = usize::MAX;
    }
}