use crate::INDENT;
use crate::ast::ty::{Type,TypeKind};

#[derive(Copy,Clone,Debug)]
pub enum InferType {
    None,
    Integer,
    Float,
    Pointer,
}
impl InferType {
    fn to_type_kind(&self) -> TypeKind {
        match self {
        InferType::None => unreachable!(),
        InferType::Float => todo!(),
        InferType::Integer => TypeKind::Integer(crate::ast::ty::IntClass::Signed(2)),
        InferType::Pointer => TypeKind::Pointer {is_const: true, inner: Box::new(Type::new_infer(crate::Span::new_null())) },
        }
    }
}
pub struct IVarEnt {
    pub ty: Type,
    pub cls: InferType,
}
impl IVarEnt {
    pub fn new() -> Self {
        IVarEnt { ty: Type::new_infer(crate::Span::new_null()), cls: InferType::None }
    }
}

pub fn get_ivar<'a>(ivars: &'a [IVarEnt], mut ty: &'a Type) -> &'a Type
{
    let mut prev_ty = ty;
    let mut seen = ::std::collections::HashSet::new();
    while let TypeKind::Infer { index, .. } = ty.kind {
        let Some(index) = index else { return prev_ty; };
        assert!( seen.insert(index), "Loop in {} at {}", prev_ty, ty );
        prev_ty = ty;
        ty = &ivars[index].ty;
    }
    ty
}

pub fn set_ivar_kind(span: &crate::Span, ivars: &mut [IVarEnt], ty: &Type, k: InferType) {
    println!("{INDENT}set_ivar_kind {ty} = {k:?}");
    match (&ty.kind,k) {
    (TypeKind::Infer { index: Some(mut i), .. }, _) => {
        while let TypeKind::Infer { index: Some(next_i), .. } = ivars[i].ty.kind {
            ivars[i].cls = k;
            i = next_i;
        }
        println!("{INDENT} _#{i} = {}", ivars[i].ty);
        if let TypeKind::Infer { index: None, .. } = ivars[i].ty.kind {
            match (ivars[i].cls, k) {
            (InferType::None, _) => { ivars[i].cls = k; },
            (InferType::Integer, InferType::Integer) => {},
            (InferType::Float, InferType::Float) => {},
            (InferType::Pointer, InferType::Pointer) => {},
            _ => panic!("{span}: Setting ivar kind on incompatible known type - {:?} != {:?}", ivars[i].cls, k),
            }
        }
        else {
            if ! ivar_kind_matches(&ivars[i].ty, k) {
                panic!("{span}: Setting ivar kind on incompatible known type - {}", ivars[i].ty);
            }
        }
        },
    _ if ivar_kind_matches(ty, k) => {},
    _ => panic!("{span}: Setting ivar kind on incompatible known type - {}", ty),
    }
}
fn ivar_kind_matches(ty: &Type, k: InferType) -> bool {
    match (&ty.kind,k) {
    (_, InferType::None) => true,
    (TypeKind::Integer(..), InferType::Integer) => true,
    (TypeKind::Pointer { .. }, InferType::Pointer) => true,
    _ => false,
    }
}

pub fn equate_types(span: &crate::Span, ivars: &mut [IVarEnt], l: &Type, r: &Type)
{
    match equate_types_inner(ivars, l, r)
    {
    Ok(_) => {},
    Err((l_i, r_i)) => {
        panic!("{span}: Type mismatch - {l_i:?} != {r_i:?}\n- {l}\n- {r}", l_i=l_i, l=FmtWithIvars(ivars,l), r=FmtWithIvars(ivars,r));
    }
    }
}

struct FmtWithIvars<'a>(&'a [IVarEnt],&'a Type);
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

fn equate_types_inner(ivars: &mut [IVarEnt], l: &Type, r: &Type) -> Result<(),(TypeKind,TypeKind)>
{
    let _i = INDENT.inc_f("equate_types", format_args!("{:?},{:?}", l, r));
    match (&l.kind, &r.kind) {
    (TypeKind::Infer { index: i1, .. }, TypeKind::Infer { index: i2, .. }) => {
        let mut i1 = i1.expect("Unspecified ivar");
        let mut i2 = i2.expect("Unspecified ivar");
        while let TypeKind::Infer { index: Some(i), .. } = ivars[i1].ty.kind {
            assert!(i2 != i, "Recursion at {:?}", ivars[i1].ty);
            println!("{INDENT}equate_types: #{} -> {}", i1, i);
            i1 = i;
        }
        while let TypeKind::Infer { index: Some(i), .. } = ivars[i2].ty.kind {
            assert!(i2 != i, "Recursion at {:?}", ivars[i2].ty);
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
        
        println!("{INDENT}equate_types(i-i): #{} {:?} = #{} {:?}", i1, t1.ty, i2, t2.ty);
        if let TypeKind::Infer { index: ref mut idx @ None, .. } = t1.ty.kind {
            if let TypeKind::Infer { index: None, .. } = t2.ty.kind {
                println!("{INDENT}equate_types(): IVar #{i1} = @#{i2}");
                match (t1.cls, t2.cls) {
                (InferType::None, InferType::None) => {},
                (InferType::Integer, InferType::Integer) => {},
                (InferType::Float, InferType::Float) => {},
                (ref mut dst @ InferType::None, i) => *dst = i,
                (i, ref mut dst @ InferType::None) => *dst = i,
                _ => return Err(( t1.cls.to_type_kind(), t2.cls.to_type_kind(), )),
                }
                *idx = Some(i2);
                Ok(())
            }
            else {
                // Shouldn't be an infer, so we can just assign into `t1`
                println!("{INDENT}equate_types(): IVar #{i1} = {:?}", t2.ty);
                if !ivar_kind_matches(&t2.ty, t1.cls) {
                    return Err(( t2.ty.kind.clone(), t1.cls.to_type_kind(), ));
                }
                t1.ty = t2.ty.clone();
                Ok(())
            }
        }
        else {
            if let TypeKind::Infer { index: None, .. } = t2.ty.kind {
                println!("{INDENT}equate_types(): IVar #{i2} = {:?}", t1.ty);
                if !ivar_kind_matches(&t1.ty, t2.cls) {
                    return Err(( t1.ty.kind.clone(), t2.cls.to_type_kind(), ));
                }
                t2.ty = t1.ty.clone();
                Ok(())
            }
            else {
                // Check the types are equal
                let t1 = t1.ty.kind.clone();
                let t2 = t2.ty.kind.clone();
                equate_type_kind(ivars, &t1, &t2)
            }
        }
        },
    (TypeKind::Infer { index, .. }, _) => {
        let mut i1 = index.expect("Unspecified ivar");
        while let TypeKind::Infer { index: Some(i), .. } = ivars[i1].ty.kind {
            i1 = i;
        }
        let t1 = &mut ivars[i1];
        println!("{INDENT}equate_types(i-c): #{} {:?} = {:?}", i1, t1.ty, r);
        if let TypeKind::Infer { index: None, .. } = t1.ty.kind {
            println!("{INDENT}equate_types(): IVar #{i1} = {:?}", r);
            t1.ty = r.clone();
            Ok(())
        }
        else {
            let v = t1.ty.kind.clone();
            equate_type_kind(ivars, &v, &r.kind)
        }
    },
    (_, TypeKind::Infer { index, .. }) => {
        let mut i2 = index.expect("Unspecified ivar");
        while let TypeKind::Infer { index: Some(i), .. } = ivars[i2].ty.kind {
            i2 = i;
        }
        let t2 = &mut ivars[i2];
        println!("{INDENT}equate_types(c-i): {:?} = #{} {:?}", l, i2, t2.ty);
        if let TypeKind::Infer { index: None, .. } = t2.ty.kind {
            println!("{INDENT}equate_types(): IVar #{i2} = {:?}", l);
            t2.ty = l.clone();
            Ok( () )
        }
        else {
            let v = t2.ty.kind.clone();
            equate_type_kind(ivars, &l.kind, &v)
        }
    },
    _ => equate_type_kind(ivars, &l.kind, &r.kind),
    }
}
fn equate_type_kind(ivars: &mut [IVarEnt], l: &TypeKind, r: &TypeKind) -> Result<(),(TypeKind,TypeKind)>
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
    (TypeKind::TypeOf(..), _) => todo!("Handle equality between typeof"),
    }
}