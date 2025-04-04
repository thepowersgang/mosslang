use crate::INDENT;
use crate::ast::path::{AbsolutePath,ValueBinding};
use crate::ast::ty::{Type,TypeKind};
use super::Revisit;
use super::ivars::{equate_types,set_ivar_kind};

pub struct IvarEnumerate<'a> {
    ivars: &'a mut Vec<super::ivars::IVarEnt>,
}
impl<'a> IvarEnumerate<'a> {
    pub fn new(ivars: &'a mut Vec<super::ivars::IVarEnt>) -> Self {
        IvarEnumerate { ivars }
    }
    pub fn fill_ivars_in(&mut self, ty: &mut crate::ast::Type) {
        match &mut ty.kind {
        TypeKind::Infer { index, kind } => {
            if index.is_none() {
                *index = Some(self.ivars.len());
                self.ivars.push(super::ivars::IVarEnt::new());
            }
        },
        TypeKind::Integer(_int_class) => {},
        TypeKind::Named(_, _) => {},
        TypeKind::Void => {},
        TypeKind::Bool => {},
        TypeKind::NullPointer => {},

        TypeKind::Tuple(items) => {
            for t in items {
                self.fill_ivars_in(t);
            }
        },
        TypeKind::Pointer { is_const: _, inner } => {
            self.fill_ivars_in(inner);
        },
        TypeKind::Array { inner, count: _ } => {
            self.fill_ivars_in(inner);
        },
        }
    }
}

impl<'a> crate::ast::ExprVisitor for IvarEnumerate<'a> {
    fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
        match expr.kind {
        crate::ast::expr::ExprKind::LiteralInteger(_, ref ty) => match ty {
            crate::ast::expr::IntLitClass::Unspecified => {
                expr.data_ty = Type { kind: TypeKind::Infer { kind: crate::ast::ty::InferKind::Integer, index: None }};
            },
            crate::ast::expr::IntLitClass::Pointer
            |crate::ast::expr::IntLitClass::Integer(_) => {
                // Handled later on, as it doesn't need to worry about generating an ivar to be filled
                },
            },
        crate::ast::expr::ExprKind::Cast(_, ref mut ty) => {
            self.fill_ivars_in(ty);
            }
        _ => {},
        }
        self.fill_ivars_in(&mut expr.data_ty);
        crate::ast::visit_mut_expr(self, expr);
    }
    
    fn visit_mut_pattern(&mut self, pat: &mut crate::ast::Pattern, refutable: bool) {
        match &mut pat.ty {
        crate::ast::PatternTy::Any => {},
        crate::ast::PatternTy::MaybeBind(_) => {}
        crate::ast::PatternTy::NamedValue(..) => {}
        crate::ast::PatternTy::Tuple(patterns) => {
            pat.data_ty = Type::new_tuple(vec![Type::new_infer(); patterns.len()]);
            self.fill_ivars_in(&mut pat.data_ty);
            for pat in patterns {
                self.visit_mut_pattern(pat, refutable);
            }
        }
        }
    }

    fn visit_mut_block(&mut self, block: &mut crate::ast::expr::Block) {
        for s in &mut block.statements {
            match s {
            crate::ast::expr::Statement::Expr(e) => self.visit_mut_expr(e),
            crate::ast::expr::Statement::Let(pattern, ty, expr) => {
                self.visit_mut_pattern(pattern, false);
                self.fill_ivars_in(ty);
                if let Some(expr) = expr {
                    self.visit_mut_expr(expr);
                }
            },
            }
        }
        if let Some(expr) = &mut block.result {
            self.visit_mut_expr(expr);
        }
    }
}

pub struct RuleEnumerate<'a, 'b> {
    pub ivars: &'a mut [super::ivars::IVarEnt],
    pub rules: &'a mut super::Rules,
    pub lc: &'b super::LookupContext,
    pub ret_ty: &'b crate::ast::Type,
    pub local_tys: &'b [crate::ast::Type],
    pub loop_stack: Vec<crate::ast::Type>,
}
impl RuleEnumerate<'_, '_> {
    fn equate_types(&mut self, span: &crate::Span, l: &crate::ast::Type, r: &crate::ast::Type) {
        equate_types(span, &mut self.ivars, l, r);
    }

    fn pattern_assign(&mut self, pattern: &crate::ast::Pattern, ty: &Type) {
        println!("{INDENT}pattern_assign: {:?} = {:?}", pattern, ty);
        for b in &pattern.bindings {
            self.equate_types(&pattern.span, &self.local_tys[b.index.unwrap() as usize], ty);
        }
        match &pattern.ty {
        crate::ast::PatternTy::Any => {},
        crate::ast::PatternTy::MaybeBind(_) => panic!("Unexpanded MaybeBind"),
        crate::ast::PatternTy::NamedValue(path, binding) => {
            let Some(binding) = binding else { panic!("Unresolved path in pattern") };

            use crate::ast::path::ValueBinding;
            let tmp_ty;
            let t = match binding {
                ValueBinding::ValueEnumVariant(absolute_path, _) => {
                    // TODO: If this is a data variant, then it should be a function pointer
                    let ap = AbsolutePath(absolute_path.0[..absolute_path.0.len()-1].to_owned());
                    let mut enum_ty = Type::new_path(crate::ast::Path { root: crate::ast::path::Root::Root, components: ap.0.clone() });
                    let TypeKind::Named(_, ref mut binding) = enum_ty.kind else { panic!(); };
                    *binding = Some(crate::ast::path::TypeBinding::ValueEnum(ap));

                    tmp_ty = enum_ty;
                    &tmp_ty
                    },
                _ => todo!("Check equality - {:?} and {:?}", ty, path),
                };
            self.equate_types(&pattern.span, t, ty);
        },
        crate::ast::PatternTy::Tuple(patterns) => {
            self.equate_types(&pattern.span, &pattern.data_ty, ty);
            let TypeKind::Tuple(tys) = &pattern.data_ty.kind else { panic!() };
            for (ty, pat) in Iterator::zip(tys.iter(), patterns.iter()) {
                self.pattern_assign(pat, ty);
            }
        },
        }
    }

    fn equate_opt_block(&mut self, span: &crate::Span, dst_ty: &Type, block: &mut Option<crate::ast::expr::Block>) {
        if let Some(block) = block {
            self.equate_block(span, dst_ty, block);
        }
        else {
            self.equate_types(span, dst_ty, &Type::new_unit());
        }
    }
    fn equate_block(&mut self, span: &crate::Span, dst_ty: &Type, block: &mut crate::ast::expr::Block) {
        if let Some(res) = &mut block.result {
            self.make_coerce(span, dst_ty.clone(), res);
        }
        else {
            match block.statements.last() {
            Some(crate::ast::expr::Statement::Expr(crate::ast::expr::Expr {
                kind:
                    |crate::ast::expr::ExprKind::Return(_)
                    |crate::ast::expr::ExprKind::Break(_)
                    |crate::ast::expr::ExprKind::Continue
                    ,
                ..
            })) => {}
            _ => {
                self.equate_types(span, dst_ty, &Type::new_unit());
            }
            }
        }
    }

    fn push_revisit(&mut self, span: &crate::Span, dst_ty: &Type, revisit: Revisit) {
        self.rules.revisits.push((span.clone(), dst_ty.clone(), revisit,));
    }
    fn make_coerce(&mut self, span: &crate::Span, dst_ty: Type, src_node: &mut crate::ast::expr::Expr) {
        let src_ty = src_node.data_ty.clone();
        let null_expr = crate::ast::expr::Expr {
            kind: crate::ast::expr::ExprKind::Continue,
            data_ty: Type::new_infer(),
            span: src_node.span.clone(),
            };
        *src_node = crate::ast::expr::Expr {
            kind: crate::ast::expr::ExprKind::Coerce(Box::new( ::std::mem::replace(src_node, null_expr) )),
            data_ty: dst_ty.clone(),
            span: span.clone(),
        };
        self.rules.revisits.push((span.clone(), dst_ty.clone(), Revisit::Coerce(src_ty),));
    }
}
impl<'a, 'b> crate::ast::ExprVisitor for RuleEnumerate<'a, 'b> {
    fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
        let _i = INDENT.inc_f("visit_expr", format_args!("{:?} -> {}", &expr.kind, expr.data_ty));

        // Loops need some special handling for `break`
        match &mut expr.kind {
        ExprKind::ForLoop { pattern: _, start, end, body, else_block } => {
            self.visit_mut_expr(start);
            self.visit_mut_expr(end);
            self.loop_stack.push(expr.data_ty.clone());
            self.visit_mut_block(body);
            self.loop_stack.pop();
            if let Some(else_block) = else_block {
                self.visit_mut_block(else_block);
            }
            },
        ExprKind::WhileLoop { cond, body, else_block } => {
            self.visit_mut_expr(cond);
            self.loop_stack.push(expr.data_ty.clone());
            self.visit_mut_block(body);
            self.loop_stack.pop();
            if let Some(else_block) = else_block {
                self.visit_mut_block(else_block);
            }
            },
        ExprKind::Loop { body } => {
            self.loop_stack.push(expr.data_ty.clone());
            self.visit_mut_block(body);
            self.loop_stack.pop();
        },
        // Everything else uses a generic vistor
        _ => crate::ast::visit_mut_expr(self, expr),
        }

        println!("{INDENT}visit_expr: {:?}", &expr.kind);
        use crate::ast::Type;
        use crate::ast::expr::ExprKind;
        match &mut expr.kind {
        ExprKind::Coerce(..) => panic!("Shouldn't be seeing coerce ops"),
        ExprKind::Block(block) => {
            if let Some(e) = &block.result {
                self.equate_types(&expr.span, &expr.data_ty, &e.data_ty);
            }
            else {
                self.equate_types(&expr.span, &expr.data_ty, &Type::new_unit());
            }
        },
        ExprKind::LiteralString(_) => {
            self.equate_types(&expr.span, &expr.data_ty, &Type::new_ptr(true, Type::new_integer(crate::ast::ty::IntClass::Signed(0))));
        },
        ExprKind::LiteralInteger(_, int_lit_class) => {
            use crate::ast::expr::IntLitClass;
            match int_lit_class {
            IntLitClass::Unspecified => set_ivar_kind(&expr.span, self.ivars, &expr.data_ty, super::ivars::InferType::Integer),
            IntLitClass::Pointer     => set_ivar_kind(&expr.span, self.ivars, &expr.data_ty, super::ivars::InferType::Pointer),
            IntLitClass::Integer(int_class) => self.equate_types(&expr.span, &expr.data_ty, &Type::new_integer(*int_class)),
            }
        }
        ExprKind::Return(value) => {
            if let Some(expr) = value {
                self.equate_types(&expr.span, self.ret_ty, &expr.data_ty);
            }
            else {
                self.equate_types(&expr.span, self.ret_ty, &Type::new_unit());
            }
            // Don't set type - this is a diverge
        },
        ExprKind::Continue => {
            // Don't set type - this is a diverge
        },
        ExprKind::Break(value) => {
            let ty = self.loop_stack.last().unwrap().clone();
            if let Some(value) = value {
                self.equate_types(&value.span, &ty, &value.data_ty);
            }
            else {
                self.equate_types(&expr.span, &ty, &Type::new_unit());
            }
            // Don't set type - this is a diverge
        },
        ExprKind::Assign { slot, op, value } => {
            self.equate_types(&expr.span, &slot.data_ty, &value.data_ty);
            self.equate_types(&expr.span, &expr.data_ty, &Type::new_unit());
            if let Some(op) = op {
            }
        },
        ExprKind::NamedValue(path, value_binding) => {
            let Some(value_binding) = value_binding else { panic!("Unresolved path: {:?}", path) };
            // Need to look up the path, to get the value's type
            let tmp_ty;
            let t = match value_binding {
                ValueBinding::Local(idx) => {
                    &self.local_tys[*idx as usize]
                },
                ValueBinding::Function(absolute_path) => {
                    //self.lc.functions.get(&absolute_path).expect("Incorrect function path")
                    todo!("Function pointer")
                },
                ValueBinding::Static(absolute_path) => {
                    &self.lc.statics.get(&absolute_path).expect("Incorrect function path")
                },
                ValueBinding::Constant(absolute_path) => {
                    &self.lc.constants.get(&absolute_path).expect("Incorrect function path")
                },
                ValueBinding::StructValue(absolute_path) => todo!(),
                ValueBinding::DataEnumVariant(absolute_path, _) => {
                    let Some(args) = self.lc.functions.get(&absolute_path) else { panic!("{}: Unable to find function for data variant {}", expr.span, absolute_path) };
                    todo!("Function pointer to enum variant")
                },
                ValueBinding::ValueEnumVariant(absolute_path, _) => {
                    let ap = absolute_path.parent();
                    let mut enum_ty = Type::new_path(crate::ast::Path { root: crate::ast::path::Root::Root, components: ap.0.clone() });
                    let TypeKind::Named(_, ref mut binding) = enum_ty.kind else { panic!(); };
                    *binding = Some(crate::ast::path::TypeBinding::ValueEnum(ap));

                    tmp_ty = enum_ty;
                    &tmp_ty
                },
                };
            self.equate_types(&expr.span, &expr.data_ty, &t);
        },
        ExprKind::CallPath(_, value_binding, args) => {
            let Some(value_binding) = value_binding else { panic!("Unresolved Callpath") };
            match value_binding {
            ValueBinding::Local(_) => todo!("call local"),
            ValueBinding::Function(absolute_path)
            |ValueBinding::StructValue(absolute_path)
            |ValueBinding::DataEnumVariant(absolute_path, _) => {
                let (ret_ty, arg_tys,is_variadic) = self.lc.functions.get(&absolute_path).unwrap();
                self.equate_types(&expr.span, &expr.data_ty, ret_ty);
                if *is_variadic {
                    if arg_tys.len() > args.len() {
                        panic!("Too few arguments to variadic function: {:?} (got {}, wanted {})", absolute_path, args.len(), arg_tys.len())
                    }
                }
                else {
                    if arg_tys.len() != args.len() {
                        panic!("Wrong argument count to function: {:?}", absolute_path)
                    }
                }
                for (i, (req_ty,arg_expr)) in Iterator::zip(arg_tys.iter(), args.iter_mut()).enumerate() {
                    println!("{INDENT}arg{} : {:?}", i, req_ty);
                    let s = arg_expr.span.clone();
                    self.make_coerce(&s, req_ty.clone(), arg_expr);
                }
            },
            ValueBinding::ValueEnumVariant(absolute_path, _) => todo!(),
            ValueBinding::Static(absolute_path) => todo!(),
            ValueBinding::Constant(absolute_path) => todo!(),
            }
        },
        ExprKind::Tuple(exprs) => {
            let ty = Type::new_tuple( exprs.iter().map(|e| e.data_ty.clone()).collect() );
            self.equate_types(&expr.span, &expr.data_ty, &ty);
        },
        ExprKind::FieldNamed(expr_v, ident) => {
            self.push_revisit(&expr.span, &expr.data_ty, Revisit::FieldNamed(expr_v.data_ty.clone(), ident.clone()));
        },
        ExprKind::FieldIndex(expr_v, idx) => {
            self.push_revisit(&expr.span, &expr.data_ty, Revisit::FieldIndex(expr_v.data_ty.clone(), *idx));
        },
        ExprKind::Index(expr_v, expr_i) => {
            self.make_coerce(&expr.span, Type::new_integer(crate::ast::ty::IntClass::PtrInt), expr_i);
            // Defer - this is a revisit
            // - Although the index should be an integer?
            self.push_revisit(&expr.span, &expr.data_ty, Revisit::Index(expr_v.data_ty.clone(), expr_i.data_ty.clone()));
        },
        ExprKind::Addr(is_mut, expr_v) => {
            let ty = Type::new_ptr( !*is_mut, expr_v.data_ty.clone() );
            self.equate_types(&expr.span, &expr.data_ty, &ty);
        },
        ExprKind::Deref(val_expr) => {
            self.push_revisit(&expr.span, &expr.data_ty, Revisit::Deref( val_expr.data_ty.clone()));
        },
        ExprKind::Cast(_expr_v, ty) => {
            // TODO: Push a rule to ensure that the cast is valid?
            // - Or, should this done in a validation pass?
            self.equate_types(&expr.span, &expr.data_ty, ty);
        },
        ExprKind::UniOp(uni_op_ty, val_expr) => {
            self.equate_types(&expr.span, &expr.data_ty, &expr.data_ty);
            self.push_revisit(&expr.span, &expr.data_ty, Revisit::UniOp(*uni_op_ty, val_expr.data_ty.clone()));
        },
        ExprKind::BinOp(bin_op_ty, expr_l, expr_r) => {
            use crate::ast::expr::BinOpTy;
            match bin_op_ty {
            // Comparisons coerce the RHS to LHS and return boolean
            BinOpTy::Equals | BinOpTy::NotEquals
            |BinOpTy::Lt | BinOpTy::LtEquals
            |BinOpTy::Gt | BinOpTy::GtEquals => {
                self.make_coerce(&expr.span, expr_l.data_ty.clone(), expr_r);
                self.equate_types(&expr.span, &expr.data_ty, &Type::new_bool());
            },
            // Boolean operators coerce both inputs too bool and return bool
            BinOpTy::BoolAnd | BinOpTy::BoolOr => {
                self.make_coerce(&expr.span, Type::new_bool(), expr_l);
                self.make_coerce(&expr.span, Type::new_bool(), expr_r);
                self.equate_types(&expr.span, &expr.data_ty, &Type::new_bool());
            },
            // Add and subtract need special logic for pointer arithmatic
            BinOpTy::Add => self.push_revisit(&expr.span, &expr.data_ty, Revisit::Add(expr_l.data_ty.clone(), expr_r.data_ty.clone())),
            BinOpTy::Sub => self.push_revisit(&expr.span, &expr.data_ty, Revisit::Sub(expr_l.data_ty.clone(), expr_r.data_ty.clone())),
            // Everything else just coerces RHS to LHS and returns LHS
            _ => {
                self.make_coerce(&expr.span, expr_l.data_ty.clone(), expr_r);
                self.equate_types(&expr.span, &expr.data_ty, &expr_l.data_ty);
            }
            }
        },
        ExprKind::CallValue(expr, exprs) => todo!("CallValue"),
        ExprKind::Loop { body } => {
            if let Some(res) = &body.result {
                self.equate_types(&expr.span, &Type::new_unit(), &res.data_ty);
            }
        },
        ExprKind::WhileLoop { cond, body, else_block } => {
            self.equate_types(&cond.span, &Type::new_bool(), &cond.data_ty);
            if let Some(res) = &body.result {
                self.equate_types(&res.span, &Type::new_unit(), &res.data_ty);
            }
            self.equate_opt_block(&expr.span, &expr.data_ty, else_block);
        },
        ExprKind::ForLoop { pattern, start, end, body, else_block } => {
            self.pattern_assign(pattern, &start.data_ty);
            self.equate_types(&start.span, &start.data_ty, &end.data_ty);

            if let Some(res) = &body.result {
                self.equate_types(&res.span, &Type::new_unit(), &res.data_ty);
            }
            self.equate_opt_block(&expr.span, &expr.data_ty, else_block);
        },
        ExprKind::IfChain { branches, else_block } => {
            for b in branches {
                self.equate_types(&b.cond.span, &Type::new_bool(), &b.cond.data_ty);
                self.equate_block(&expr.span, &expr.data_ty, &mut b.body);
            }
            self.equate_opt_block(&expr.span, &expr.data_ty, else_block);
        },
        ExprKind::Match { value, branches } => {
            for b in branches {
                self.pattern_assign(&b.pat, &value.data_ty);
                self.equate_types(&expr.span, &expr.data_ty, &b.val.data_ty);
            }
        },
        }
    }

    fn visit_mut_block(&mut self, block: &mut crate::ast::expr::Block) {
        for s in &mut block.statements {
            match s {
            crate::ast::expr::Statement::Expr(e) => {
                self.visit_mut_expr(e);
                // Coerce to void, discarding the value?
                self.make_coerce(&e.span.clone(), Type::new_void(), e);
                // Or coerce to unit
                //self.make_coerce(&e.span.clone(), Type::new_unit(), e);
            },
            crate::ast::expr::Statement::Let(pattern, ty, expr) => {
                self.visit_mut_pattern(pattern, false);
                self.pattern_assign(pattern, ty);
                if let Some(expr) = expr {
                    self.visit_mut_expr(expr);
                    self.equate_types(&expr.span, ty, &expr.data_ty);
                }
            },
            }
        }
        if let Some(expr) = &mut block.result {
            self.visit_mut_expr(expr);
        }
    }
}