//! Apply pre-compilation rules
//! 
//! Currently just `cfg` flags
// cspell:ignore krate

pub fn expand_crate(krate: &mut super::ast::Crate)
{
    check_attrs(&mut krate.attributes);
    expand_module(&mut krate.module);
}
fn expand_module(module: &mut super::ast::items::Module) {
    
    println!("expand_module: {} items", module.items.len());
    module.items.retain_mut(|v| check_attrs(&mut v.attributes));
    println!("expand_module: {} items", module.items.len());

    for v in &mut module.items {
        use crate::ast::items::ItemType;
        match &mut v.ty {
        //crate::ast::items::ItemType::Module(module) => expand_module(module),
        ItemType::ExternBlock(eb) => {
            eb.items.retain_mut(|v| check_attrs(&mut v.attributes));
        },
        ItemType::TypeAlias(_) => {},
        ItemType::Struct(str) => {
            str.fields.retain_mut(|v| check_attrs(&mut v.attributes));
        },
        ItemType::Enum(enm) => {
            enm.variants.retain_mut(|v| check_attrs(&mut v.attributes));
            for v in &mut enm.variants {
                match &mut v.ty {
                crate::ast::items::EnumVariantTy::Bare => {},
                crate::ast::items::EnumVariantTy::Value(expr_root) => {
                    expand_expr(expr_root)
                    },
                crate::ast::items::EnumVariantTy::Data(_) => todo!(),
                }
            }
        },
        ItemType::Union(_) => {
        },
        ItemType::Function(function) => {
            expand_expr(&mut function.code);
        },
        ItemType::Static(s) => {
            expand_expr(&mut s.value);
        },
        ItemType::Constant(i) => {
            expand_expr(&mut i.value);
        },
        }
    }
}

fn expand_expr(_root: &mut crate::ast::ExprRoot) {
}

fn check_attrs(attrs: &mut Vec<crate::ast::Attribute>) -> bool {
    let mut cfg_fail = false;
    attrs.retain_mut(|a| {
        if a.name == "cfg" {
            let crate::ast::AttributeData::SubItems(ref i) = a.data else {
                todo!("Error for malformed `cfg` attribute: {:?}", a)
            };
            if i.len() != 1 {
                todo!("Error for malformed `cfg` attribute: {:?}", a)
            }
            cfg_fail |= !check_cfg(&i[0]);
            false
        }
        else if a.name == "cfg_attr" {
            let crate::ast::AttributeData::SubItems(ref mut i) = a.data else {
                todo!("Error for malformed `cfg` attribute: {:?}", a)
            };
            if i.len() != 2 {
                todo!("Error for malformed `cfg_attr` attribute: {:?}", a)
            }
            let new_a = i.pop().unwrap();
            let cfg_a = i.pop().unwrap();
            if !check_cfg(&cfg_a) {
                false
            }
            else {
                *a = new_a;
                true
            }
        }
        else {
            true
        }
    });
    ! cfg_fail
}
fn check_cfg(a: &crate::ast::Attribute) -> bool {
    match &a.data {
    crate::ast::AttributeData::None => {
        // Flag check
        if a.name == "false" {
            return false;
        }
        else if a.name == "true" {
            true
        }
        else {
            // TODO: Search a list of global flags
            todo!("Check cfg(foo)")
        }
    }
    crate::ast::AttributeData::Value(_str_val) => {
        // Value check
        todo!("Check cfg(foo=\"bar\")")
    }
    crate::ast::AttributeData::SubItems(attributes) => {
        if a.name == "any" {
            for sa in attributes {
                if check_cfg(sa) {
                    return true
                }
            }
            false
        }
        else if a.name == "all" {
            for sa in attributes {
                if !check_cfg(sa) {
                    return false
                }
            }
            true
        }
        else {
            todo!("Unknown cfg combinator")
        }
    }
    }
}