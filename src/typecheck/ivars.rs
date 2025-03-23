use crate::INDENT;
use crate::ast::ty::{Type,TypeKind};

pub fn get_ivar<'a>(ivars: &'a [Type], mut ty: &'a Type) -> &'a Type
{
    let mut prev_ty = ty;
    let mut seen = ::std::collections::HashSet::new();
    while let TypeKind::Infer { index, .. } = ty.kind {
        let Some(index) = index else { return prev_ty; };
        assert!( seen.insert(index), "Loop in {} at {}", prev_ty, ty );
        prev_ty = ty;
        ty = &ivars[index];
    }
    ty
}

pub fn equate_types(span: &crate::Span, ivars: &mut [Type], l: &Type, r: &Type)
{
    match equate_types_inner(ivars, l, r)
    {
    Ok(_) => {},
    Err((l_i, r_i)) => {
        panic!("{span}: Type mismatch - {l_i:?} != {r_i:?}\n- {l}\n- {r}", l_i=l_i, l=FmtWithIvars(ivars,l), r=FmtWithIvars(ivars,r));
    }
    }
}

struct FmtWithIvars<'a>(&'a [Type],&'a Type);
impl<'a> ::std::fmt::Display for FmtWithIvars<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ty = get_ivar(self.0, self.1);
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
            println!("{INDENT}equate_types(): IVar #{i1} = {:?}", r);
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
            println!("{INDENT}equate_types(): IVar #{i2} = {:?}", l);
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
    (TypeKind::Void, _) => Err((l.clone(),r.clone())),
    (TypeKind::Bool, TypeKind::Bool) => Ok( () ),
    (TypeKind::Bool, _) => Err((l.clone(),r.clone())),
    (TypeKind::Integer(ic_l), TypeKind::Integer(ic_r)) if ic_l == ic_r => Ok(()),
    (TypeKind::Integer { .. }, _) => Err((l.clone(),r.clone())),
    (TypeKind::Tuple(inner_l), TypeKind::Tuple(inner_r)) => {
        if inner_l.len() != inner_r.len() {
            return Err((l.clone(),r.clone()));
        }
        for (l,r) in Iterator::zip(inner_l.iter(), inner_r.iter()) {
            equate_types_inner(ivars, l, r)?;
        }
        Ok(())
    },
    (TypeKind::Tuple { .. }, _) => Err((l.clone(),r.clone())),
    (TypeKind::Named(_, binding_l), TypeKind::Named(_, binding_r)) => {
        if binding_l != binding_r {
            return Err((l.clone(),r.clone()));
        }
        Ok( () )
    },
    (TypeKind::Named { .. }, _) => Err((l.clone(),r.clone())),
    (TypeKind::Pointer { is_const, inner }, TypeKind::Pointer { is_const: ic_r, inner: i_r }) => {
        if *is_const != *ic_r {
            return Err((l.clone(),r.clone()));
        }
        equate_types_inner(ivars, inner, i_r)
    },
    (TypeKind::Pointer { .. }, _) => Err((l.clone(),r.clone())),
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
    (TypeKind::Array { .. }, _) => Err((l.clone(),r.clone())),
    }
}