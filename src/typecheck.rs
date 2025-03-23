use crate::INDENT;
use crate::ast::path::AbsolutePath;
use crate::ast::path::ValueBinding;
use crate::ast::{Type,ty::TypeKind};

#[derive(Default)]
struct LookupContext {
    statics: ::std::collections::HashMap<AbsolutePath, Type>,
    constants: ::std::collections::HashMap<AbsolutePath, Type>,
    functions: ::std::collections::HashMap<AbsolutePath, (Type,Vec<Type>,bool)>,

    fields: ::std::collections::HashMap<AbsolutePath, ::std::collections::HashMap<crate::Ident,Type> >,
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
        ItemType::Struct(s) => {
            let fields = s.fields.iter().map(|v| (v.name.clone(), v.ty.clone())).collect();
            lc.fields.insert(
                path.append(v.name.as_ref().unwrap().clone()),
                fields
            );
        },
        ItemType::Enum(_enm) => {
        },
        ItemType::Union(u) => {
            //let fields = u.fields.iter().map(|v| (v.name, v.ty.clone())).collect();
            //lc.fields.insert(
            //    path.append(v.name.as_ref().unwrap().clone()),
            //    fields
            //);
        },
        ItemType::Function(function) => {
            let k = path.append(v.name.as_ref().unwrap().clone());
            let v = (function.sig.ret.clone(), function.sig.args.iter().map(|(_,t)| t.clone()).collect(), function.sig.is_variadic);
            println!("fn {k:?} = {v:?}");
            lc.functions.insert(k,v);
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
        let mut es = IvarEnumerate { ivars: &mut ivars };
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
    let mut rules = Rules::default();
    {
        let mut ss: RuleEnumerate<'_, '_> = RuleEnumerate {
            ivars: &mut ivars,
            lc, ret_ty, local_tys: &mut expr.variables,
            rules: &mut rules,
            loop_stack: Vec::new()
        };
        crate::ast::visit_mut_expr(&mut ss, &mut expr.e);
    }

    #[derive(Default)]
    struct Rules {
        revisits: Vec<(crate::Span,Type,Revisit)>,
    }
    /// Ivar possiblity rules
    struct IvarRules {
    }
    impl IvarRules {
        fn coerce_to(&mut self, idx: usize, ty: Type) {
        }
        fn coerce_from(&mut self, idx: usize, ty: Type) {
        }
    }
    let mut ir = IvarRules {};

    fn get_ivar<'a>(ivars: &'a [Type], mut ty: &'a Type) -> &'a Type {
        let mut prev_ty = ty;
        while let TypeKind::Infer { index, .. } = ty.kind {
            let Some(index) = index else { return prev_ty; };
            prev_ty = ty;
            ty = &ivars[index];
        }
        ty
    }

    // Run solver
    while !rules.revisits.is_empty() {
        let n = rules.revisits.len();
        rules.revisits.retain(|(span, dst_ty, op)| {
            println!("{INDENT}revisit {dst_ty:?} = {op:?}");
            enum R {
                Keep,
                Consume,
            }
            let r = match op {
                Revisit::Coerce(src_ty) => {
                    let t_l = get_ivar(&ivars, dst_ty);
                    let t_r = get_ivar(&ivars, src_ty);
                    match (&t_l.kind, &t_r.kind) {
                    _ if ::std::ptr::eq(t_l, t_r) => R::Consume,
                    (TypeKind::Infer { index: i_l, .. }, TypeKind::Infer { index: i_r, .. }) => {
                        ir.coerce_from(i_l.unwrap(), t_r.clone());
                        ir.coerce_to(i_r.unwrap(), t_l.clone());
                        R::Keep
                        },
                    (TypeKind::Infer { index: i_l, .. }, _) => {
                        ir.coerce_from(i_l.unwrap(), t_r.clone());
                        R::Keep
                        },
                    (_, TypeKind::Infer { index: i_r, .. }) => {
                        ir.coerce_to(i_r.unwrap(), t_l.clone());
                        R::Keep
                        },
                    (TypeKind::Integer(ic_l), TypeKind::Integer(ic_r)) => {
                        if ic_l != ic_r {
                            // TODO: Check if this coerce is valid
                            //match (ic_l, ic_r) {
                            //()
                            //}
                        }
                        R::Consume
                        },
                    (TypeKind::Pointer { is_const: c_l, inner: i_l }, TypeKind::Pointer { is_const: c_r, inner: i_r }) => {
                        // If the source is a constant pointer, the destination must be a constant
                        if *c_r && !*c_l {
                            // Type error
                            equate_types(span, &mut ivars, dst_ty, src_ty);
                            unreachable!();
                        }
                        let inner_dst = get_ivar(&ivars, i_l);
                        let inner_src = get_ivar(&ivars, i_r);
                        match (&inner_dst.kind,&inner_src.kind) {
                        (TypeKind::Infer { index: Some(idx), .. }, _) => {
                            ir.coerce_from(*idx, inner_src.clone());
                            R::Keep
                        },
                        (_, TypeKind::Infer { index: Some(idx), .. }) => {
                            ir.coerce_to(*idx, inner_dst.clone());
                            R::Keep
                        },
                        (TypeKind::Void, _) => R::Consume,
                        (_, TypeKind::Void) => R::Consume,
                        _ => {
                            // Equate the inners, as the constness may have changed
                            let i_l = (**i_l).clone();
                            let i_r = (**i_r).clone();
                            equate_types(span, &mut ivars, &i_l, &i_r);
                            R::Consume
                        }
                        }
                        },
                    _ => todo!("{span}: {} := {}", t_l, t_r),
                    }
                },
                Revisit::Deref(inner_ty) =>
                    match &get_ivar(&ivars, inner_ty).kind {
                    TypeKind::Infer { .. } => R::Keep,
                    TypeKind::Pointer { inner, .. } => {
                        let inner = (*inner).clone();
                        equate_types(span, &mut ivars, dst_ty, &inner);
                        R::Consume
                    }
                    _ => panic!("{span}: Type error: Deref on unsupported type {:?}", inner_ty),
                    },
                Revisit::Index(val_ty, _index_ty) => match &get_ivar(&ivars, val_ty).kind {
                    TypeKind::Infer { .. } => R::Keep,

                    TypeKind::Pointer { inner, .. }
                    | TypeKind::Array { inner, .. } => {
                        let inner = (*inner).clone();
                        equate_types(span, &mut ivars, dst_ty, &inner);
                        //equate_types(&mut ivars, &Type::new_integer(crate::ast::ty::IntClass::PtrInt), index_ty);
                        R::Consume
                    }
                    _ => panic!("{span}: Type error: Index on unsupported type {:?}", val_ty),
                    },
                Revisit::FieldNamed(ty, name) => {
                    use crate::ast::path::TypeBinding;
                    let ty = get_ivar(&ivars, ty);
                    match &ty.kind {
                    TypeKind::Infer { .. } => R::Keep,
                    TypeKind::Named(_, Some(TypeBinding::Alias(_))) => panic!("Unresolved type alias - {}", ty),
                    TypeKind::Named(_, Some(TypeBinding::Union(p))) => todo!("Field from union - {}", ty),
                    TypeKind::Named(_, Some(TypeBinding::Struct(p))) => {
                        let Some(f) = lc.fields.get(p) else { panic!("{span}: BUG: No fields on {}", ty) };
                        let Some(fld_ty) = f.get(name) else {
                            panic!("{span}: No field {} on type {}", name, ty);
                        };
                        equate_types(span, &mut ivars, dst_ty, fld_ty);
                        R::Consume
                    },
                    _ => panic!("{span}: Getting field from invalid type - {}", ty),
                    }
                },
                Revisit::FieldIndex(_, _) => todo!("field index"),
                Revisit::BinOp(ty_l, bin_op_ty, ty_r) => {
                    use crate::ast::expr::BinOpTy;
                    match bin_op_ty {
                    // Arithmatic operations yield the promoted type of the two
                    BinOpTy::Add
                    |BinOpTy::Sub
                    |BinOpTy::Mul
                    |BinOpTy::Div
                    |BinOpTy::Rem => {
                        //let ty_l = get_ivar(&ivars, ty_l);
                        //let ty_r = get_ivar(&ivars, ty_r);
                        //todo!("binop {} and {}", ty_l, ty_r)
                        equate_types(span, &mut ivars, ty_l, ty_r);
                        equate_types(span, &mut ivars, dst_ty, ty_l);
                        R::Consume
                    }

                    // Bitwise requires equal
                    BinOpTy::BitAnd
                    |BinOpTy::BitOr
                    |BinOpTy::BitXor => {
                        equate_types(span, &mut ivars, ty_l, ty_r);
                        equate_types(span, &mut ivars, dst_ty, ty_l);
                        R::Consume
                    },
                    // Shifts yield the LHS type, RHS must be an integer
                    BinOpTy::Shl|BinOpTy::Shr => {
                        equate_types(span, &mut ivars, dst_ty, ty_l);
                        R::Consume
                    },
                    
                    // For comparisons, types must be equal. Result is bool
                    BinOpTy::Equals
                    |BinOpTy::NotEquals
                    |BinOpTy::Lt | BinOpTy::LtEquals
                    |BinOpTy::Gt | BinOpTy::GtEquals => {
                        equate_types(span, &mut ivars, ty_l, ty_r);
                        equate_types(span, &mut ivars, dst_ty, &Type::new_bool());
                        R::Consume
                    },

                    // Boolean operations require integer inputs? Result is a bool
                    BinOpTy::BoolAnd|BinOpTy::BoolOr => {
                        // TODO: Check for integer or pointer input (or force bool and add coercions)
                        equate_types(span, &mut ivars, dst_ty, &Type::new_bool());
                        R::Consume
                    },
                    }
                    },
                Revisit::UniOp(uni_op_ty, in_ty) => {
                    match uni_op_ty
                    {
                    crate::ast::expr::UniOpTy::Invert => {
                        equate_types(span, &mut ivars, dst_ty, in_ty);
                        R::Consume
                    },
                    crate::ast::expr::UniOpTy::Negate => {
                        equate_types(span, &mut ivars, dst_ty, in_ty);
                        R::Consume
                    },
                    }
                },
                };
            matches!(r, R::Keep)
        });
        assert!(rules.revisits.len() < n);
    }
    
    struct FmtWithIvars<'a>(&'a [Type],&'a Type);
    impl<'a> ::std::fmt::Display for FmtWithIvars<'a> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mut ty = self.1;
            while let TypeKind::Infer { index, .. } = ty.kind {
                ty = &self.0[index.unwrap()];
            }
            match &ty.kind {
            TypeKind::Tuple(items) => {
                f.write_str("(")?;
                for inner in items {
                    FmtWithIvars(self.0, inner).fmt(f)?;
                    f.write_str(", ")?;
                }
                f.write_str(")")
            },
            TypeKind::Pointer { is_const, inner } => {
                if *is_const {
                    f.write_str("*const ")?;
                }
                else {
                    f.write_str("*mut ")?;
                }
                FmtWithIvars(self.0, inner).fmt(f)
            },
            TypeKind::Array { inner, count } => {
                f.write_str("[")?;
                FmtWithIvars(self.0, inner).fmt(f)?;
                f.write_str("; ")?;
                match count {
                crate::ast::ty::ArraySize::Unevaluated(_) => todo!(),
                crate::ast::ty::ArraySize::Known(v) => write!(f, "{}", v)?,
                }
                f.write_str("]")
            },
            _ => ty.fmt(f),
            }
        }
    }
    fn equate_types(span: &crate::Span, ivars: &mut [Type], l: &Type, r: &Type)
    {
        match equate_types_inner(ivars, l, r)
        {
        Ok(_) => {},
        Err((l_i, r_i)) => {
            panic!("{span}: Type mismatch - {l_i:?} != {r_i:?}\n- {l}\n- {r}", l_i=l_i, l=FmtWithIvars(ivars,l), r=FmtWithIvars(ivars,r));
        }
        }
    }
    fn equate_types_inner(ivars: &mut [Type], l: &Type, r: &Type) -> Result<(),(TypeKind,TypeKind)>
    {
        let _i = INDENT.inc_f("equate_types", format_args!("{:?},{:?}", l, r));
        match (&l.kind, &r.kind) {
        (TypeKind::Infer { index: i1, .. }, TypeKind::Infer { index: i2, .. }) => {
            let mut i1 = i1.expect("Unspecified ivar");
            let mut i2 = i2.expect("Unspecified ivar");
            while let TypeKind::Infer { index: Some(i), .. } = ivars[i1].kind {
                assert!(i2 != i, "Recursion at {:?}", ivars[i1]);
                println!("{INDENT}equate_types: #{} -> {}", i1, i);
                i1 = i;
            }
            while let TypeKind::Infer { index: Some(i), .. } = ivars[i2].kind {
                assert!(i2 != i, "Recursion at {:?}", ivars[i2]);
                println!("{INDENT}equate_types: {} -> {}", i2, i);
                i2 = i;
            }
            if i1 == i2 {
                return Ok(());
            }
            let (t1,t2) = if i1 < i2 {
                    let (a,b) = ivars.split_at_mut(i2);
                    (&mut a[i1], &mut b[0])
                }
                else {
                    let (a,b) = ivars.split_at_mut(i1);
                    (&mut b[0], &mut a[i2], )
                };
            println!("{INDENT}equate_types(i-i): #{} {:?} = #{} {:?}", i1, t1, i2, t2);
            if let TypeKind::Infer { index: ref mut idx @ None, .. } = t1.kind {
                if let TypeKind::Infer { index: None, .. } = t2.kind {
                    println!("{INDENT}equate_types(): IVar #{i1} = @#{i2}");
                    *idx = Some(i2);
                    Ok(())
                }
                else {
                    // Shouldn't be an infer, so we can just assign into `t1`
                    println!("{INDENT}equate_types(): IVar #{i1} = {:?}", t2);
                    *t1 = t2.clone();
                    Ok(())
                }
            }
            else {
                if let TypeKind::Infer { index: None, .. } = t2.kind {
                    println!("{INDENT}equate_types(): IVar #{i2} = {:?}", t1);
                    *t2 = t1.clone();
                    Ok(())
                }
                else {
                    // Check the types are equal
                    let t1 = t1.kind.clone();
                    let t2 = t2.kind.clone();
                    equate_type_kind(ivars, &t1, &t2)
                }
            }
            },
        (TypeKind::Infer { index, .. }, _) => {
            let mut i1 = index.expect("Unspecified ivar");
            while let TypeKind::Infer { index: Some(i), .. } = ivars[i1].kind {
                i1 = i;
            }
            let t1 = &mut ivars[i1];
            println!("{INDENT}equate_types(i-c): #{} {:?} = {:?}", i1, t1, r);
            if let TypeKind::Infer { index: None, .. } = t1.kind {
                *t1 = r.clone();
                Ok(())
            }
            else {
                let v = t1.kind.clone();
                equate_type_kind(ivars, &v, &r.kind)
            }
        },
        (_, TypeKind::Infer { index, .. }) => {
            let mut i2 = index.expect("Unspecified ivar");
            while let TypeKind::Infer { index: Some(i), .. } = ivars[i2].kind {
                i2 = i;
            }
            let t2 = &mut ivars[i2];
            println!("{INDENT}equate_types(c-i): {:?} = #{} {:?}", l, i2, t2);
            if let TypeKind::Infer { index: None, .. } = t2.kind {
                *t2 = l.clone();
                Ok( () )
            }
            else {
                let v = t2.kind.clone();
                equate_type_kind(ivars, &l.kind, &v)
            }
        },
        _ => equate_type_kind(ivars, &l.kind, &r.kind),
        }
    }
    fn equate_type_kind(ivars: &mut [Type], l: &TypeKind, r: &TypeKind) -> Result<(),(TypeKind,TypeKind)>
    {
        match (l,r) {
        (TypeKind::Infer { .. }, _) => panic!(),
        (TypeKind::Void, TypeKind::Void) => Ok( () ),
        (TypeKind::Integer(ic_l), TypeKind::Integer(ic_r)) if ic_l == ic_r => Ok(()),
        (TypeKind::Tuple(inner_l), TypeKind::Tuple(inner_r)) => {
            if inner_l.len() != inner_r.len() {
                return Err((l.clone(),r.clone()));
            }
            for (l,r) in Iterator::zip(inner_l.iter(), inner_r.iter()) {
                equate_types_inner(ivars, l, r)?;
            }
            Ok(())
        },
        (TypeKind::Named(_, binding_l), TypeKind::Named(_, binding_r)) => {
            if binding_l != binding_r {
                return Err((l.clone(),r.clone()));
            }
            Ok( () )
        },
        (TypeKind::Pointer { is_const, inner }, TypeKind::Pointer { is_const: ic_r, inner: i_r }) => {
            if *is_const != *ic_r {
                return Err((l.clone(),r.clone()));
            }
            equate_types_inner(ivars, inner, i_r)
        },
        (TypeKind::Array { inner: i_l, count: c_l }, TypeKind::Array { inner: i_r, count: c_r }) => {
            equate_types_inner(ivars, i_l, i_r)?;
            use crate::ast::ty::ArraySize;
            match (c_l, c_r) {
            (ArraySize::Unevaluated(se_l), ArraySize::Unevaluated(se_r)) => todo!(),
            (ArraySize::Unevaluated(se_l), ArraySize::Known(s_r)) => todo!(),
            (ArraySize::Known(s_l), ArraySize::Unevaluated(se_r)) => todo!(),
            (ArraySize::Known(s_l), ArraySize::Known(s_r)) => {
                if s_l != s_r {
                    return Err((l.clone(),r.clone()));
                }
            },
            }
            Ok( () )
        },
        _ => Err((l.clone(),r.clone())),
        }
    }

    struct IvarEnumerate<'a> {
        ivars: &'a mut Vec<crate::ast::Type>,
    }
    impl<'a> IvarEnumerate<'a> {
        fn fill_ivars_in(&mut self, ty: &mut crate::ast::Type) {
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
    impl<'a> crate::ast::ExprVisitor for IvarEnumerate<'a> {
        fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
            self.fill_ivars_in(&mut expr.data_ty);
            match expr.kind {
            crate::ast::expr::ExprKind::LiteralInteger(_, crate::ast::expr::IntLitClass::Unspecified) => {
                // TODO: Can this create an ivar that is integer-only?
            },
            crate::ast::expr::ExprKind::LiteralInteger(_, crate::ast::expr::IntLitClass::Pointer) => {
                expr.data_ty = Type::new_ptr(false, expr.data_ty.clone());
                },
            crate::ast::expr::ExprKind::Cast(_, ref mut ty) => {
                self.fill_ivars_in(ty);
                }
            _ => {},
            }
            crate::ast::visit_mut_expr(self, expr);
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

    struct RuleEnumerate<'a, 'b> {
        ivars: &'a mut [crate::ast::Type],
        rules: &'a mut Rules,
        lc: &'b LookupContext,
        ret_ty: &'b crate::ast::Type,
        local_tys: &'b [crate::ast::Type],
        loop_stack: Vec<crate::ast::Type>,
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
                    ValueBinding::EnumVariant(absolute_path, _) => {
                        // TODO: If this is a data variant, then it should be a function pointer
                        let ap = AbsolutePath(absolute_path.0[..absolute_path.0.len()-1].to_owned());
                        let mut enum_ty = Type::new_path(crate::ast::Path { root: crate::ast::path::Root::Root, components: ap.0.clone() });
                        let TypeKind::Named(_, ref mut binding) = enum_ty.kind else { panic!(); };
                        *binding = Some(crate::ast::path::TypeBinding::Enum(ap));

                        tmp_ty = enum_ty;
                        &tmp_ty
                        },
                    _ => todo!("Check equality - {:?} and {:?}", ty, path),
                    };
                self.equate_types(&pattern.span, t, ty);
            },
            crate::ast::PatternTy::Tuple(patterns) => todo!("Handle tuple patterns, force into inference"),
            }
        }

        fn equate_opt_block(&mut self, span: &crate::Span, dst_ty: &Type, block: &Option<crate::ast::expr::Block>) {
            if let Some(block) = block {
                self.equate_block(span, dst_ty, block);
            }
            else {
                self.equate_types(span, dst_ty, &Type::new_unit());
            }
        }
        fn equate_block(&mut self, span: &crate::Span, dst_ty: &Type, block: &crate::ast::expr::Block) {
            if let Some(res) = &block.result {
                self.equate_types(span, dst_ty, &res.data_ty);
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
                self.equate_types(&expr.span, &expr.data_ty, &match int_lit_class {
                    crate::ast::expr::IntLitClass::Unspecified => return,
                    crate::ast::expr::IntLitClass::Pointer => return,
                    crate::ast::expr::IntLitClass::Integer(int_class) => Type::new_integer(*int_class),
                    });
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
                    ValueBinding::EnumVariant(absolute_path, _) => {
                        // TODO: If this is a data variant, then it should be a function pointer
                        let ap = AbsolutePath(absolute_path.0[..absolute_path.0.len()-1].to_owned());
                        let mut enum_ty = Type::new_path(crate::ast::Path { root: crate::ast::path::Root::Root, components: ap.0.clone() });
                        let TypeKind::Named(_, ref mut binding) = enum_ty.kind else { panic!(); };
                        *binding = Some(crate::ast::path::TypeBinding::Enum(ap));

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
                ValueBinding::Function(absolute_path) => {
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
                        self.make_coerce(&expr.span, req_ty.clone(), arg_expr);
                    }
                },
                ValueBinding::Static(absolute_path) => todo!(),
                ValueBinding::Constant(absolute_path) => todo!(),
                ValueBinding::StructValue(absolute_path) => todo!(),
                ValueBinding::EnumVariant(absolute_path, _) => todo!(),
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
            ExprKind::Cast(expr_v, ty) => {
                self.equate_types(&expr.span, &expr.data_ty, ty);
            },
            ExprKind::UniOp(uni_op_ty, val_expr) => {
                self.equate_types(&expr.span, &expr.data_ty, &expr.data_ty);
                self.push_revisit(&expr.span, &expr.data_ty, Revisit::UniOp(*uni_op_ty, val_expr.data_ty.clone()));
            },
            ExprKind::BinOp(bin_op_ty, expr_l, expr_r) => {
                self.push_revisit(&expr.span, &expr.data_ty, Revisit::BinOp(expr_l.data_ty.clone(), *bin_op_ty, expr_r.data_ty.clone()));
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
                self.equate_opt_block(&expr.span, &expr.data_ty, &else_block);
            },
            ExprKind::ForLoop { pattern, start, end, body, else_block } => {
                self.pattern_assign(pattern, &start.data_ty);
                self.equate_types(&start.span, &start.data_ty, &end.data_ty);

                if let Some(res) = &body.result {
                    self.equate_types(&res.span, &Type::new_unit(), &res.data_ty);
                }
                self.equate_opt_block(&expr.span, &expr.data_ty, &else_block);
            },
            ExprKind::IfChain { branches, else_block } => {
                for b in branches {
                    self.equate_types(&b.cond.span, &Type::new_bool(), &b.cond.data_ty);
                    self.equate_block(&expr.span, &expr.data_ty, &b.body);
                }
                self.equate_opt_block(&expr.span, &expr.data_ty, &else_block);
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
                crate::ast::expr::Statement::Expr(e) => self.visit_mut_expr(e),
                crate::ast::expr::Statement::Let(pattern, ty, expr) => {
                    self.visit_mut_pattern(pattern, false);
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

    #[derive(Debug)]
    enum Revisit {
        Coerce(Type),
        Deref(Type),
        Index(Type, Type),
        FieldNamed(Type, crate::Ident),
        FieldIndex(Type, usize),
        BinOp(Type, crate::ast::expr::BinOpTy, Type),
        UniOp(crate::ast::expr::UniOpTy, Type),
    }
}