use crate::INDENT;
use crate::ast::path::AbsolutePath;
use crate::ast::path::ValueBinding;
use crate::ast::Type;

#[derive(Default)]
struct LookupContext {
    statics: ::std::collections::HashMap<AbsolutePath, Type>,
    constants: ::std::collections::HashMap<AbsolutePath, Type>,
    functions: ::std::collections::HashMap<AbsolutePath, (Type,Vec<Type>,bool)>,
}

pub fn typecheck(ast_crate: &mut crate::ast::Crate)
{
    println!("typecheck");

    let mut lc = LookupContext::default();
    enumerate_mod(&mut lc, &ast_crate.module, &AbsolutePath(Vec::new()));

    typecheck_mod(&lc, &mut ast_crate.module)
}
fn enumerate_mod(lc: &mut LookupContext, module: &crate::ast::items::Module, path: &AbsolutePath)
{
    for v in &module.items {
        use crate::ast::items::ItemType;
        match &v.ty {
        //ItemType::Module(module) => enumerate_mod(lc, module),
        ItemType::ExternBlock(eb) => {
            for i in &eb.items {
                use crate::ast::items::ExternItemType;
                match &i.ty {
                ExternItemType::Function(function_signature) => {
                    lc.functions.insert(
                        path.append(i.name.clone()),
                        (function_signature.ret.clone(), function_signature.args.iter().map(|(_,t)| t.clone()).collect(), function_signature.is_variadic)
                    );
                },
                ExternItemType::Static(extern_static) => {
                    lc.statics.insert(
                        path.append(i.name.clone()),
                        extern_static.ty.clone(),
                    );
                }
                }
            }
        },
        ItemType::TypeAlias(_ty) => {
        },
        ItemType::Struct(_str) => {
        },
        ItemType::Enum(_enm) => {
        },
        ItemType::Union(_) => {
        },
        ItemType::Function(function) => {
            lc.functions.insert(
                path.append(v.name.as_ref().unwrap().clone()),
                (function.sig.ret.clone(), function.sig.args.iter().map(|(_,t)| t.clone()).collect(), function.sig.is_variadic)
            );
        },
        ItemType::Static(i) => {
            lc.statics.insert(
                path.append(v.name.as_ref().unwrap().clone()),
                i.ty.clone(),
            );
        },
        ItemType::Constant(i) => {
            lc.constants.insert(
                path.append(v.name.as_ref().unwrap().clone()),
                i.ty.clone(),
            );
        },
        }
    }
}
fn typecheck_mod(lc: &LookupContext, module: &mut crate::ast::items::Module)
{
    let _i = INDENT.inc("typecheck_mod");
    
    for v in &mut module.items {
        use crate::ast::items::ItemType;
        match &mut v.ty {
        //crate::ast::items::ItemType::Module(module) => typecheck_mod(module),
        ItemType::ExternBlock(_eb) => {
        },
        ItemType::TypeAlias(_ty) => {
        },
        ItemType::Struct(_str) => {
        },
        ItemType::Enum(enm) => {
            let ty = crate::ast::Type::new_integer(crate::ast::ty::IntClass::Signed(2));
            for v in &mut enm.variants {
                match &mut v.ty {
                crate::ast::items::EnumVariantTy::Bare => {},
                crate::ast::items::EnumVariantTy::Value(expr_root) => {
                    typecheck_expr(lc, &ty, expr_root, &mut [])
                    },
                crate::ast::items::EnumVariantTy::Named(_) => {},
                }
            }
        },
        ItemType::Union(_) => {
        },
        ItemType::Function(function) => {
            println!("{INDENT}resolve_mod: Function {}", v.name.as_ref().unwrap());
            typecheck_expr(lc, &function.sig.ret, &mut function.code, &mut function.sig.args);
        },
        ItemType::Static(i) => {
            println!("{INDENT}typecheck_mod: Static {}", v.name.as_ref().unwrap());
            typecheck_expr(lc, &i.ty, &mut i.value, &mut []);
        },
        ItemType::Constant(i) => {
            println!("{INDENT}typecheck_mod: Constant {}", v.name.as_ref().unwrap());
            typecheck_expr(lc, &i.ty, &mut i.value, &mut []);
        },
        }
    }
}


fn typecheck_expr(lc: &LookupContext, ret_ty: &crate::ast::Type, expr: &mut crate::ast::ExprRoot, args: &mut [(crate::ast::Pattern, crate::ast::Type)])
{
    // Enumerate ivars
    let mut ivars = Vec::new();

    {
        let mut es = EnumerateState { ivars: &mut ivars };
        expr.variables.reserve(expr.variable_count);
        for (_, t) in args {
            expr.variables.push(t.clone());
        }
        expr.variables.resize_with(expr.variable_count, || Type::new_infer());
        for t in &mut expr.variables {
            es.fill_ivars_in(t);
        }
        crate::ast::visit_mut_expr(&mut es, &mut expr.e);
    }
    // Assign/solve
    let mut ss = SolveState {
        ivars: &mut ivars,
        lc, ret_ty, local_tys: &mut expr.variables,
        revisits: Vec::new(),
        loop_stack: Vec::new()
    };
    crate::ast::visit_mut_expr(&mut ss, &mut expr.e);
    
    struct EnumerateState<'a> {
        ivars: &'a mut Vec<crate::ast::Type>,
    }
    impl<'a> EnumerateState<'a> {
        fn fill_ivars_in(&mut self, ty: &mut crate::ast::Type) {
            use crate::ast::ty::TypeKind;
            match &mut ty.kind {
            TypeKind::Infer { explicit: _, index } => {
                if index.is_none() {
                    *index = Some(self.ivars.len());
                    self.ivars.push(crate::ast::Type::new_infer());
                }
            },
            TypeKind::Integer(_int_class) => {},
            TypeKind::Named(_, _) => {},
            TypeKind::Void => {},

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
    impl<'a> crate::ast::ExprVisitor for EnumerateState<'a> {
        fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
            self.fill_ivars_in(&mut expr.data_ty);
            if let crate::ast::expr::ExprKind::LiteralInteger(_, crate::ast::expr::IntLitClass::Unspecified) = expr.kind {
                //expr.data_ty.kind
            }
            else if let crate::ast::expr::ExprKind::Cast(_, ref mut ty) = expr.kind {
                self.fill_ivars_in(ty);
            }
            crate::ast::visit_mut_expr(self, expr);
        }
    
        fn visit_mut_block(&mut self, block: &mut crate::ast::expr::Block) {
            crate::ast::visit_mut_block(self, block);
        }
    }

    struct SolveState<'a, 'b> {
        ivars: &'a mut Vec<crate::ast::Type>,
        lc: &'b LookupContext,
        ret_ty: &'b crate::ast::Type,
        local_tys: &'b [crate::ast::Type],
        loop_stack: Vec<crate::ast::Type>,
        revisits: Vec<(Type, Revisit,)>,
    }
    impl SolveState<'_, '_> {
        fn equate_types(&mut self, l: &crate::ast::Type, r: &crate::ast::Type) {
            println!("{INDENT}equate_types({:?},{:?})", l, r);
            use crate::ast::ty::TypeKind;
            match (&l.kind, &r.kind) {
            (TypeKind::Infer { index: i1, .. }, TypeKind::Infer { index: i2, .. }) => {
                let mut i1 = i1.expect("Unspecified ivar");
                let mut i2 = i2.expect("Unspecified ivar");
                while let TypeKind::Infer { index: Some(i), .. } = self.ivars[i1].kind {
                    assert!(i2 != i, "Recursion at {:?}", self.ivars[i1]);
                    println!("{INDENT}equate_types: {} -> {}", i1, i);
                    i1 = i;
                }
                while let TypeKind::Infer { index: Some(i), .. } = self.ivars[i2].kind {
                    assert!(i2 != i, "Recursion at {:?}", self.ivars[i2]);
                    println!("{INDENT}equate_types: {} -> {}", i2, i);
                    i2 = i;
                }
                if i1 == i2 {
                    return ;
                }
                let (t1,t2) = if i1 < i2 {
                        let (a,b) = self.ivars.split_at_mut(i2);
                        (&mut a[i1], &mut b[0])
                    }
                    else {
                        let (a,b) = self.ivars.split_at_mut(i1);
                        (&mut b[0], &mut a[i2], )
                    };
                println!("{INDENT}equate_types(i-i): #{} {:?} = #{} {:?}", i1, t1, i2, t2);
                if let TypeKind::Infer { index: ref mut idx @ None, .. } = t1.kind {
                    if let TypeKind::Infer { index: None, .. } = t2.kind {
                        println!("{INDENT}equate_types(): IVar #{i1} = @#{i2}");
                        *idx = Some(i2);
                    }
                    else {
                        // Shouldn't be an infer, so we can just assign into `t1`
                        println!("{INDENT}equate_types(): IVar #{i1} = {:?}", t2);
                        *t1 = t2.clone();
                    }
                }
                else {
                    if let TypeKind::Infer { index: None, .. } = t2.kind {
                        println!("{INDENT}equate_types(): IVar #{i2} = {:?}", t1);
                        *t2 = t1.clone();
                    }
                    else {
                        // Check the types are equal
                        let t1 = t1.kind.clone();
                        let t2 = t2.kind.clone();
                        self.equate_types_inner(&t1, &t2)
                    }
                }
                },
            (TypeKind::Infer { index, .. }, _) => {
                let mut i1 = index.expect("Unspecified ivar");
                while let TypeKind::Infer { index: Some(i), .. } = self.ivars[i1].kind {
                    i1 = i;
                }
                let t1 = &mut self.ivars[i1];
                if let TypeKind::Infer { index: None, .. } = t1.kind {
                    *t1 = r.clone();
                }
                else {
                    let v = t1.kind.clone();
                    self.equate_types_inner(&v, &r.kind)
                }
            },
            (_, TypeKind::Infer { index, .. }) => {
                let mut i2 = index.expect("Unspecified ivar");
                while let TypeKind::Infer { index: Some(i), .. } = self.ivars[i2].kind {
                    i2 = i;
                }
                let t2 = &mut self.ivars[i2];
                if let TypeKind::Infer { index: None, .. } = t2.kind {
                    *t2 = l.clone();
                }
                else {
                    let v = t2.kind.clone();
                    self.equate_types_inner(&l.kind, &v)
                }
            },
            _ => self.equate_types_inner(&l.kind, &r.kind),
            }
        }
        fn equate_types_inner(&mut self, l: &crate::ast::ty::TypeKind, r: &crate::ast::ty::TypeKind) {
            use crate::ast::ty::TypeKind;
            match l {
            TypeKind::Infer { .. } => panic!(),
            TypeKind::Void => if let TypeKind::Void = r {
            }
            else {
                panic!("Type mismatch: {:?} != {:?}", l, r)
            },
            TypeKind::Integer(ic_l) => if let TypeKind::Integer(ic_r) = r {
                if ic_l != ic_r {
                    panic!("Type mismatch: {:?} != {:?}", l, r)
                } else {
                    // Allowed
                }
            }
            else {
                panic!("Type mismatch: {:?} != {:?}", l, r)
            },
            TypeKind::Tuple(inner_l) => if let TypeKind::Tuple(inner_r) = r {
                if inner_l.len() != inner_r.len() {
                    panic!("Type mismatch: {:?} != {:?}", l, r)
                }
                for (l,r) in Iterator::zip(inner_l.iter(), inner_r.iter()) {
                    self.equate_types(l, r);
                }
            }
            else {
                panic!("Type mismatch: {:?} != {:?}", l, r)
            },
            TypeKind::Named(_, binding_l) => if let TypeKind::Named(_, binding_r) = r {
                if binding_l != binding_r {
                    panic!("Type mismatch: {:?} != {:?}", l, r)
                }
            }
            else {
                panic!("Type mismatch: {:?} != {:?}", l, r)
            },
            TypeKind::Pointer { is_const, inner } => if let TypeKind::Pointer { is_const: ic_r, inner: i_r } = r {
                if *is_const != *ic_r {
                    panic!("Type mismatch: {:?} != {:?}", l, r)
                }
                self.equate_types(inner, i_r);
            }
            else {
                panic!("Type mismatch: {:?} != {:?}", l, r)
            },
            TypeKind::Array { inner, count: c_l } => if let TypeKind::Array { inner: i_r, count: c_r } = r {
                self.equate_types(inner, i_r);
                use crate::ast::ty::ArraySize;
                match (c_l, c_r) {
                (ArraySize::Unevaluated(se_l), ArraySize::Unevaluated(se_r)) => todo!(),
                (ArraySize::Unevaluated(se_l), ArraySize::Known(s_r)) => todo!(),
                (ArraySize::Known(s_l), ArraySize::Unevaluated(se_r)) => todo!(),
                (ArraySize::Known(s_l), ArraySize::Known(s_r)) => {
                    if s_l != s_r {
                        panic!("Type mismatch: {:?} != {:?}", l, r)
                    }
                },
                }
            }
            else {
                panic!("Type mismatch: {:?} != {:?}", l, r)
            },
            }
        }

        fn pattern_assign(&mut self, pattern: &crate::ast::Pattern, ty: &Type) {
            for b in &pattern.bindings {
                self.equate_types(&self.local_tys[b.index.unwrap() as usize], ty);
            }
            match &pattern.ty {
            crate::ast::PatternTy::Any => {},
            crate::ast::PatternTy::MaybeBind(_) => panic!("Unexpanded MaybeBind"),
            crate::ast::PatternTy::NamedValue(path) => todo!("Check equality - {:?} and {:?}", ty, path),
            crate::ast::PatternTy::Tuple(patterns) => todo!("Handle tuple patterns, force into inference"),
            }
        }

        fn equate_opt_block(&mut self, dst_ty: &Type, block: &Option<crate::ast::expr::Block>) {
            if let Some(block) = block {
                self.equate_block(dst_ty, block);
            }
            else {
                self.equate_types(dst_ty, &Type::new_unit());
            }
        }
        fn equate_block(&mut self, dst_ty: &Type, block: &crate::ast::expr::Block) {
            if let Some(res) = &block.result {
                self.equate_types(dst_ty, &res.data_ty);
            }
            else {
                self.equate_types(dst_ty, &Type::new_unit());
            }
        }
    }
    impl<'a, 'b> crate::ast::ExprVisitor for SolveState<'a, 'b> {
        fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
            let _i = INDENT.inc_f("visit_expr", format_args!("{:?}", &expr.kind));

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
            ExprKind::Block(block) => {
                if let Some(e) = &block.result {
                    self.equate_types(&expr.data_ty, &e.data_ty);
                }
                else {
                    self.equate_types(&expr.data_ty, &&Type::new_unit());
                }
            },
            ExprKind::LiteralString(_) => {
                self.equate_types(&expr.data_ty, &Type::new_ptr(true, Type::new_integer(crate::ast::ty::IntClass::Signed(0))));
            },
            ExprKind::LiteralInteger(_, int_lit_class) => {
                self.equate_types(&expr.data_ty, &match int_lit_class {
                    crate::ast::expr::IntLitClass::Unspecified => return,
                    crate::ast::expr::IntLitClass::Pointer => Type::new_ptr(true, Type::new_infer()),
                    crate::ast::expr::IntLitClass::Integer(int_class) => Type::new_integer(*int_class),
                    });
            }
            ExprKind::Return(expr) => {
                if let Some(expr) = expr {
                    self.equate_types(self.ret_ty, &expr.data_ty);
                }
                else {
                    self.equate_types(self.ret_ty, &Type::new_unit());
                }
                // Don't set type?
            },
            ExprKind::Continue => {
                // Don't set type?
            },
            ExprKind::Break(expr) => {
                let ty = self.loop_stack.last().unwrap().clone();
                if let Some(expr) = expr {
                    self.equate_types(&ty, &expr.data_ty);
                }
                else {
                    self.equate_types(&ty, &Type::new_unit());
                }

            },
            ExprKind::Assign { slot, op, value } => {
                self.equate_types(&slot.data_ty, &value.data_ty);
                self.equate_types(&expr.data_ty, &Type::new_unit());
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
                    ValueBinding::EnumVariant(absolute_path, _) => {
                        // TODO: If this is a data variant, then it should be a function pointer
                        let ap = AbsolutePath(absolute_path.0[..absolute_path.0.len()-1].to_owned());
                        let mut enum_ty = Type::new_path(crate::ast::Path { root: crate::ast::path::Root::Root, components: ap.0.clone() });
                        let crate::ast::ty::TypeKind::Named(_, ref mut binding) = enum_ty.kind else { panic!(); };
                        *binding = Some(crate::ast::path::TypeBinding::Enum(ap));

                        tmp_ty = enum_ty;
                        &tmp_ty
                    },
                    };
                self.equate_types(&expr.data_ty, &t);
            },
            ExprKind::CallPath(_, value_binding, args) => {
                let Some(value_binding) = value_binding else { panic!("Unresolved Callpath") };
                match value_binding {
                ValueBinding::Local(_) => todo!("call local"),
                ValueBinding::Function(absolute_path) => {
                    let (ret_ty, arg_tys,is_variadic) = self.lc.functions.get(&absolute_path).unwrap();
                    self.equate_types(&expr.data_ty, ret_ty);
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
                    for (req_ty,arg_expr) in Iterator::zip(arg_tys.iter(), args.iter()) {
                        println!("{:?}", req_ty);
                        self.equate_types(req_ty, &arg_expr.data_ty);
                    }
                },
                ValueBinding::Static(absolute_path) => todo!(),
                ValueBinding::Constant(absolute_path) => todo!(),
                ValueBinding::StructValue(absolute_path) => todo!(),
                ValueBinding::EnumVariant(absolute_path, _) => todo!(),
                }
            },
            ExprKind::Tuple(exprs) => todo!(),
            ExprKind::FieldNamed(expr_v, ident) => {
                self.revisits.push((expr.data_ty.clone(), Revisit::FieldNamed(expr_v.data_ty.clone(), ident.clone()),));
            },
            ExprKind::FieldIndex(expr_v, idx) => {
                self.revisits.push((expr.data_ty.clone(), Revisit::FieldIndex(expr_v.data_ty.clone(), *idx),));
            },
            ExprKind::Index(expr_v, expr_i) => {
                // Defer - this is a revisit
                // - Although the index should be an integer?
                self.revisits.push((expr.data_ty.clone(), Revisit::Index(expr_v.data_ty.clone(), expr_i.data_ty.clone()),));
            },
            ExprKind::Addr(_, expr) => todo!("addr"),
            ExprKind::Deref(val_expr) => {
                self.revisits.push((expr.data_ty.clone(), Revisit::Deref( val_expr.data_ty.clone()),));
            },
            ExprKind::Cast(expr_v, ty) => {
                self.equate_types(&expr.data_ty, ty);
            },
            ExprKind::UniOp(uni_op_ty, val_expr) => {
                self.equate_types(&expr.data_ty, &expr.data_ty);
                self.revisits.push((expr.data_ty.clone(), Revisit::UniOp(*uni_op_ty, val_expr.data_ty.clone()),));
            },
            ExprKind::BinOp(bin_op_ty, expr_l, expr_r) => {
                self.revisits.push((expr.data_ty.clone(), Revisit::BinOp(expr_l.data_ty.clone(), *bin_op_ty, expr_r.data_ty.clone()),));
            },
            ExprKind::CallValue(expr, exprs) => todo!(),
            ExprKind::Loop { body } => {
                if let Some(res) = &body.result {
                    self.equate_types(&Type::new_unit(), &res.data_ty);
                }
            },
            ExprKind::WhileLoop { cond, body, else_block } => {
                self.equate_types(&Type::new_bool(), &cond.data_ty);
                if let Some(res) = &body.result {
                    self.equate_types(&Type::new_unit(), &res.data_ty);
                }
                self.equate_opt_block(&expr.data_ty, &else_block);
            },
            ExprKind::ForLoop { pattern, start, end, body, else_block } => {
                self.pattern_assign(pattern, &start.data_ty);
                self.equate_types(&start.data_ty, &end.data_ty);

                if let Some(res) = &body.result {
                    self.equate_types(&Type::new_unit(), &res.data_ty);
                }
                self.equate_opt_block(&expr.data_ty, &else_block);
            },
            ExprKind::IfChain { branches, else_block } => {
                for b in branches {
                    self.equate_types(&Type::new_bool(), &b.cond.data_ty);
                    self.equate_block(&expr.data_ty, &b.body);
                }
                self.equate_opt_block(&expr.data_ty, &else_block);
            },
            ExprKind::Match { value, branches } => todo!("match"),
            }
        }
    
        fn visit_mut_block(&mut self, block: &mut crate::ast::expr::Block) {
            crate::ast::visit_mut_block(self, block);
        }
    }

    enum Revisit {
        Deref(Type),
        Index(Type, Type),
        FieldNamed(Type, crate::Ident),
        FieldIndex(Type, usize),
        BinOp(Type, crate::ast::expr::BinOpTy, Type),
        UniOp(crate::ast::expr::UniOpTy, Type),
    }
}