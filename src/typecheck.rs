use crate::INDENT;
use crate::ast::path::AbsolutePath;
use crate::ast::ty::{Type,TypeKind,InferKind};

mod enumerate;
mod ivars;

use self::ivars::{equate_types,get_ivar};

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
        ItemType::Union(_u) => {
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
    let root_span = expr.e.span.clone();
    // Enumerate ivars
    let mut ivars = Vec::new();

    {
        let mut es = enumerate::IvarEnumerate::new(&mut ivars);
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
        let mut ss = enumerate::RuleEnumerate {
            ivars: &mut ivars,
            lc,
            ret_ty,
            local_tys: &mut expr.variables,
            rules: &mut rules,

            loop_stack: Vec::new()
        };
        crate::ast::visit_mut_expr(&mut ss, &mut expr.e);
    }

    /// Ivar possiblity rules
    #[derive(Default)]
    struct IvarRules {
        ivars: std::collections::HashMap<usize, ::std::collections::BTreeSet<(Type, bool)>>,
    }
    impl IvarRules {
        fn coerce_to(&mut self, idx: usize, ty: Type) {
            let i = self.ivars.entry(idx).or_default();
            i.insert((ty, true));
        }
        fn coerce_from(&mut self, idx: usize, ty: Type) {
            let i = self.ivars.entry(idx).or_default();
            i.insert((ty, false));
        }
    }
    let mut ir = IvarRules::default();

    // Run solver
    for pass_num in 0 .. {
        for (i,ty) in ivars.iter().enumerate() {
            println!("{INDENT} _#{} = {}", i, ty);
        }
        if rules.revisits.is_empty() {
            break;
        }
        let _ih = INDENT.inc_f("revisit", format_args!("pass {}", pass_num));
        let n = rules.revisits.len();
        rules.revisits.retain(|(span, dst_ty, op)| {
            println!("{INDENT}revisit {dst_ty:?} = {op:?} @ {span}");
            enum R {
                Keep,
                Consume,
            }
            let r = match op {
                Revisit::Coerce(src_ty) => {
                    let t_l = get_ivar(&ivars, dst_ty);
                    let t_r = get_ivar(&ivars, src_ty);
                    println!("Revisit::Coerce: {} := {}", t_l, t_r);
                    match (&t_l.kind, &t_r.kind) {
                    _ if ::std::ptr::eq(t_l, t_r) => R::Consume,
                    _ if t_l == t_r => R::Consume,
                    (TypeKind::Infer { index: i_l, kind: InferKind::None }, TypeKind::Infer { index: i_r, kind: InferKind::None }) => {
                        ir.coerce_from(i_l.unwrap(), t_r.clone());
                        ir.coerce_to(i_r.unwrap(), t_l.clone());
                        R::Keep
                        },
                    (TypeKind::Infer { index: i_l, kind: InferKind::None }, _) => {
                        ir.coerce_from(i_l.unwrap(), t_r.clone());
                        R::Keep
                        },
                    (_, TypeKind::Infer { index: i_r, kind: InferKind::None }) => {
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
                    (TypeKind::Pointer { .. }, TypeKind::NullPointer) => {
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
                        // Void is special, it can coerce to/from anything
                        (TypeKind::Void, _) => R::Consume,
                        // TODO: Wait, is coercing from `void` good? It's what C does, but is it a good idea?
                        // Answer: Nope. Let's force explicit casts
                        // Except, `NULL` is `*mut void` allowing it to coerce everywhere
                        // - Should probably make a `nullptr_t` type instead
                        (_, TypeKind::Void) => R::Consume,
                        _ => {
                            // Equate the inners, as the constness may have changed
                            // TODO: For better error messages, create a suitable type using the LHS's constness
                            let i_l = (**i_l).clone();
                            let i_r = (**i_r).clone();
                            equate_types(span, &mut ivars, &i_l, &i_r);
                            R::Consume
                        }
                        }
                        },
                    _ => {
                        equate_types(span, &mut ivars, dst_ty, src_ty);
                        R::Consume
                        }
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
                    TypeKind::Named(_, Some(TypeBinding::Union(p) | TypeBinding::Struct(p))) => {
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
                Revisit::FieldIndex(ty, idx) => {
                    use crate::ast::path::TypeBinding;
                    let ty = get_ivar(&ivars, ty);
                    match &ty.kind {
                    TypeKind::Infer { .. } => R::Keep,
                    TypeKind::Named(_, Some(TypeBinding::Alias(_))) => panic!("Unresolved type alias - {}", ty),
                    TypeKind::Tuple(flds) => {
                        let Some(fld_ty) = flds.get(*idx) else {
                            panic!("{span}: No field index {} on type {}", idx, ty);
                        };
                        let fld_ty = fld_ty.clone();
                        equate_types(span, &mut ivars, dst_ty, &fld_ty);
                        R::Consume
                    },
                    _ => panic!("{span}: Getting field from invalid type - {}", ty),
                    }
                },
                Revisit::Add(ty_l, ty_r) => {
                    println!("Revisit::Add: {} + {}", get_ivar(&ivars, ty_l), get_ivar(&ivars, ty_r));
                    let ty_li = get_ivar(&ivars, ty_l);
                    let ty_ri = get_ivar(&ivars, ty_r);
                    match (&ty_li.kind, &ty_ri.kind) {
                    (TypeKind::Infer { kind: InferKind::None, .. }, _) => R::Keep,
                    (TypeKind::Pointer { .. }, TypeKind::Infer { kind: InferKind::None, .. }) => R::Keep,
                    (TypeKind::Pointer { .. }, TypeKind::Infer { kind: InferKind::Integer, .. })
                    |(TypeKind::Pointer { .. }, TypeKind::Integer(..)) => {
                        equate_types(span, &mut ivars, ty_r, &Type::new_integer(crate::ast::ty::IntClass::PtrInt));
                        equate_types(span, &mut ivars, dst_ty, ty_l);
                        R::Consume
                    },
                    (TypeKind::Infer { kind: InferKind::Integer, .. }, _)
                    | (TypeKind::Integer(..), _) => {
                        equate_types(span, &mut ivars, ty_l, ty_r);
                        equate_types(span, &mut ivars, dst_ty, ty_l);
                        R::Consume
                    },
                    _ => todo!("Add {ty_li} + {ty_ri}"),
                    }
                    },
                Revisit::Sub(ty_l, ty_r) => {
                    println!("Revisit::Sub: {} - {}", get_ivar(&ivars, ty_l), get_ivar(&ivars, ty_r));
                    let ty_li = get_ivar(&ivars, ty_l);
                    let ty_ri = get_ivar(&ivars, ty_r);
                    match (&ty_li.kind, &ty_ri.kind) {
                    (TypeKind::Infer { kind: InferKind::None, .. }, _) => R::Keep,
                    (TypeKind::Pointer { .. }, TypeKind::Infer { kind: InferKind::None, .. }) => R::Keep,
                    (TypeKind::Pointer { .. }, TypeKind::Pointer { .. }) => {
                        equate_types(span, &mut ivars, ty_l, ty_r);
                        equate_types(span, &mut ivars, dst_ty, &Type::new_integer(crate::ast::ty::IntClass::PtrDiff));
                        R::Consume
                    },
                    (TypeKind::Pointer { .. }, TypeKind::Infer { kind: InferKind::Integer, .. })
                    | (TypeKind::Pointer { .. }, TypeKind::Integer(..)) => {
                        equate_types(span, &mut ivars, ty_r, &Type::new_integer(crate::ast::ty::IntClass::PtrInt));
                        equate_types(span, &mut ivars, dst_ty, ty_l);
                        R::Consume
                    },
                    (TypeKind::Infer { kind: InferKind::Integer, .. }, _)
                    |(TypeKind::Integer(..), _) => {
                        equate_types(span, &mut ivars, ty_l, ty_r);
                        equate_types(span, &mut ivars, dst_ty, ty_l);
                        R::Consume
                    },
                    _ => todo!("Sub {ty_li} - {ty_ri}"),
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
        let mut changed = rules.revisits.len() < n;
        if !changed {
            for (idx, v) in &mut ir.ivars {
                if v.len() == 0 {
                    continue;
                }
                let TypeKind::Infer { index: None, .. } = ivars[*idx].kind else {
                    // Known
                    v.clear();
                    continue;
                };
                let dst_ty = Type { kind: TypeKind::Infer { kind: InferKind::None, index: Some(*idx) } };
                println!("{INDENT} #{}: {:?}", idx, v);
                
                fn find_single(v: &std::collections::BTreeSet<(Type,bool)>, req_is_to: bool) -> Option<&Type> {
                    let mut rv = None;
                    for (ty,is_to) in v.iter() {
                        if *is_to == req_is_to {
                            if rv.is_some() {
                                return None;
                            }
                            rv = Some(ty);
                        }
                    }
                    return rv;
                }
                if let Some(ty) = find_single(v, true) {
                    println!("{INDENT} IVar #{} = {} (single to)", idx, ty);
                    equate_types(&root_span, &mut ivars, &dst_ty, ty);
                    changed = true;
                    continue ;
                }
                if let Some(ty) = find_single(v, false) {
                    println!("{INDENT} IVar #{} = {} (single from)", idx, ty);
                    equate_types(&root_span, &mut ivars, &dst_ty, ty);
                    changed = true;
                    continue ;
                }
            }
        }
        assert!(changed);
    }
    
}

#[derive(Debug)]
enum Revisit {
    Coerce(Type),
    Deref(Type),
    Index(Type, Type),
    FieldNamed(Type, crate::Ident),
    FieldIndex(Type, usize),
    Add(Type, Type),
    Sub(Type, Type),
    UniOp(crate::ast::expr::UniOpTy, Type),
}
#[derive(Default)]
struct Rules {
    revisits: Vec<(crate::Span,Type,Revisit)>,
}
