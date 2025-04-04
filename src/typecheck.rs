use crate::INDENT;
use crate::ast::path::AbsolutePath;
use crate::ast::ty::{Type,TypeKind};

mod enumerate;
mod ivars;
mod run;
mod commit;

use self::run::typecheck_expr;

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
        ItemType::Enum(enm) => {
            let enm_path = path.append(v.name.as_ref().unwrap().clone());
            let enm_ty = Type { kind: TypeKind::Named(
                crate::ast::Path { root: crate::ast::path::Root::Root, components: enm_path.0.clone() },
                Some(crate::ast::path::TypeBinding::DataEnum(enm_path.clone()))
                ) };
            for variant in &enm.variants {
                if let crate::ast::items::EnumVariantTy::Data(ty) = &variant.ty {
                    lc.functions.insert(
                        enm_path.append(variant.name.clone()),
                        (enm_ty.clone(), vec![ty.clone()], false)
                    );
                }
            }
        },
        ItemType::Union(u) => {
            let fields = u.variants.iter().map(|v| (v.name.clone(), v.ty.clone())).collect();
            lc.fields.insert(
                path.append(v.name.as_ref().unwrap().clone()),
                fields
            );
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
                crate::ast::items::EnumVariantTy::Data(_) => {},
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
