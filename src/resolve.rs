use crate::ast::path::ValueBinding;
use crate::ast::path::TypeBinding;
use crate::ast::path::AbsolutePath;
use std::collections::HashMap;

pub fn resolve(ast_crate: &mut crate::ast::Crate)
{
    println!("resolve");
    resolve_mod(&mut ast_crate.module)
}
pub fn resolve_mod(module: &mut crate::ast::items::Module)
{
    println!("resolve_mod: {} items", module.items.len());

    // Make a list of items for use by inner resolve
    let item_scope = {
        let get_ap = |name| {
            AbsolutePath(vec![name])
        };
        let mut item_scope = ItemScope {
            types: Default::default(),
            values: Default::default(),
        };
        for v in &module.items {
            use crate::ast::items::ItemType;
            match &v.ty {
            //crate::ast::items::ItemType::Module(module) => expand_module(module),
            ItemType::ExternBlock(eb) => {
                for i in &eb.items {
                    match i.ty {
                    crate::ast::items::ExternItemType::Function(_) => {
                        item_scope.values.insert(i.name.clone(), ValueBinding::Function(get_ap(i.name.clone())));
                    },
                    crate::ast::items::ExternItemType::Static(_) => {
                        item_scope.values.insert(i.name.clone(), ValueBinding::Static(get_ap(i.name.clone())));
                    },
                    };
                }
                },
            ItemType::Enum(enm) => {
                let name = v.name.clone().unwrap();
                let vi = enm.variants.iter().enumerate().map(|(i, v)| (v.name.clone(), (i, match v.ty {
                    crate::ast::items::EnumVariantTy::Bare => VariantInfo::Value,
                    crate::ast::items::EnumVariantTy::Value(_) => VariantInfo::Value,
                    crate::ast::items::EnumVariantTy::Named(_) => VariantInfo::Type,
                    },),)).collect();
                item_scope.types.insert(name.clone(), (TypeBinding::Enum(get_ap(name)), Some(vi)));
            },
            ItemType::TypeAlias(_) => {
                let name = v.name.clone().unwrap();
                item_scope.types.insert(name.clone(), (TypeBinding::Alias(get_ap(name)), None,));
            },
            ItemType::Struct(_) => {
                let name = v.name.clone().unwrap();
                item_scope.types.insert(name.clone(), (TypeBinding::Struct(get_ap(name)),None,));
            },
            ItemType::Union(_) => {
                let name = v.name.clone().unwrap();
                item_scope.types.insert(name.clone(), (TypeBinding::Union(get_ap(name)),None,));
            },
            ItemType::Function(_) => {
                let name = v.name.clone().unwrap();
                item_scope.values.insert(name.clone(), ValueBinding::Function(get_ap(name)));
            },
            ItemType::Static(_) => {
                let name = v.name.clone().unwrap();
                item_scope.values.insert(name.clone(), ValueBinding::Static(get_ap(name)));
            },
            ItemType::Constant(_) => {
                    let name = v.name.clone().unwrap();
                    item_scope.values.insert(name.clone(), ValueBinding::Constant(get_ap(name)));
            },
            };
        }
        item_scope
    };

    for v in &mut module.items {
        use crate::ast::items::ItemType;
        match &mut v.ty {
        //crate::ast::items::ItemType::Module(module) => expand_module(module),
        ItemType::ExternBlock(eb) => {
            for v in &mut eb.items {
                match &mut v.ty {
                crate::ast::items::ExternItemType::Function(function_signature) => {
                    for a in &mut function_signature.args {
                        resolve_type(&item_scope, &mut a.1);
                    }
                    resolve_type(&item_scope, &mut function_signature.ret);
                },
                crate::ast::items::ExternItemType::Static(extern_static) => {
                    resolve_type(&item_scope, &mut extern_static.ty);
                },
                }
            }
        },
        ItemType::TypeAlias(ty) => {
            resolve_type(&item_scope, ty);
        },
        ItemType::Struct(str) => {
            for f in &mut str.fields {
                resolve_type(&item_scope, &mut f.ty);
            }
        },
        ItemType::Enum(enm) => {
            for v in &mut enm.variants {
                match &mut v.ty {
                crate::ast::items::EnumVariantTy::Bare => {},
                crate::ast::items::EnumVariantTy::Value(expr_root) => {
                    resolve_expr(&item_scope, expr_root, &mut [])
                    },
                crate::ast::items::EnumVariantTy::Named(_) => todo!(),
                }
            }
        },
        ItemType::Union(_) => {
        },
        ItemType::Function(function) => {
            for a in &mut function.sig.args {
                resolve_type(&item_scope, &mut a.1);
            }
            resolve_type(&item_scope, &mut function.sig.ret);
            resolve_expr(&item_scope, &mut function.code, &mut function.sig.args);
        },
        ItemType::Static(s) => {
            resolve_type(&item_scope, &mut s.ty);
            resolve_expr(&item_scope, &mut s.value, &mut []);
        },
        ItemType::Constant(i) => {
            resolve_type(&item_scope, &mut i.ty);
            resolve_expr(&item_scope, &mut i.value, &mut []);
        },
        }
    }
}

enum VariantInfo {
    Value,
    Type,
}
struct ItemScope {
    types: HashMap<crate::Ident, (crate::ast::path::TypeBinding, Option<HashMap<crate::Ident, (usize, VariantInfo,)>>,)>,
    values: HashMap<crate::Ident, crate::ast::path::ValueBinding>,
}

fn resolve_type(item_scope: &ItemScope, ty: &mut crate::ast::Type)
{
}

fn resolve_expr(item_scope: &ItemScope, expr: &mut crate::ast::ExprRoot, args: &mut [(crate::ast::Pattern, crate::ast::Type)])
{
    println!("resolve_expr");
    let mut c = Context {
        item_scope,
        next_index: 0,
        layers: Vec::new(),
    };
    if ! args.is_empty() {
        c.layers.push(Default::default());
        for a in args {
            crate::ast::ExprVisitor::visit_mut_pattern(&mut c, &mut a.0, false);
        }
    }
    crate::ast::visit_mut_expr(&mut c, &mut expr.e);


    #[derive(Default)]
    struct ContextLayer {
        names: ::std::collections::HashMap<crate::Ident, usize>,
    }
    struct Context<'a> {
        item_scope: &'a ItemScope,
        next_index: usize,
        layers: Vec<ContextLayer>,
    }
    impl Context<'_> {
        fn define_var(&mut self, name: &crate::Ident) {
            if let Some(l) = self.layers.last_mut() {
                l.names.insert(name.clone(), self.next_index);
                self.next_index += 1;
            }
        }

        fn resolve_path_value(&self, p: &mut crate::ast::Path) -> crate::ast::path::ValueBinding {
            assert!(matches!(p.root, crate::ast::path::Root::None));
            let c = p.components.first().expect("Empty path?");
            if p.components.len() == 2 {
                let vn = &p.components[1];
                if let Some((ap, Some(variants))) = self.item_scope.types.get(c) {
                    let TypeBinding::Enum(ap) = ap else { unreachable!() };
                    if let Some((idx, VariantInfo::Value)) = variants.get(vn) {
                        return crate::ast::path::ValueBinding::EnumVariant(ap.append(vn.clone()), *idx)
                    }
                }
            }
            if p.components.len() == 1 {
                for layer in self.layers.iter().rev() {
                    if let Some(v) = layer.names.get(c) {
                        return crate::ast::path::ValueBinding::Local(*v);
                    }
                }
                if let Some(v) = self.item_scope.values.get(c) {
                    return v.clone();
                }
            }
            todo!("Resolve path {:?}", p);
        }
    }
    impl crate::ast::ExprVisitor for Context<'_> {
        fn visit_mut_pattern(&mut self, p: &mut crate::ast::Pattern, refutable: bool) {
            println!("Patern: {:?}", p);
            for b in &p.bindings {
                self.define_var(b);
            }
            use crate::ast::PatternTy;
            match &mut p.ty {
            PatternTy::Any => {},
            PatternTy::MaybeBind(ident) => {
                if refutable {
                    // TODO: look up globals
                    if let Some(_v) = self.item_scope.values.get(ident) {
                        p.ty = PatternTy::NamedValue(crate::ast::Path {
                            root: crate::ast::path::Root::None,
                            components: vec![ident.clone()],
                        });
                        return ;
                    }
                }
                p.bindings.push(ident.clone());
                self.define_var(ident);
                p.ty = PatternTy::Any;
            },
            PatternTy::NamedValue(_) => {},
            PatternTy::Tuple(patterns) => {
                for p in patterns {
                    self.visit_mut_pattern(p, refutable);
                }
            },
            }
        }
        fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
            use crate::ast::expr::ExprKind;
            match &mut expr.kind {
            ExprKind::CallPath(p, ..) => {
                self.resolve_path_value(p);
                },
            ExprKind::NamedValue(p) => {
                self.resolve_path_value(p);
                },
            _ => {},
            }
            // Special case for blocks that generate scopes without being blocks
            match &mut expr.kind {
            ExprKind::Match { value, branches } => {
                crate::ast::visit_mut_expr(self, value);
                for a in branches {
                    self.layers.push(ContextLayer::default());
                    self.visit_mut_pattern(&mut a.pat, true);
                    crate::ast::visit_mut_expr(self, &mut a.val);
                    self.layers.pop();
                }
            }
            ExprKind::ForLoop { pattern, start, end, body, else_block } => {
                self.layers.push(ContextLayer::default());
                self.visit_mut_pattern(pattern, false);
                crate::ast::visit_mut_expr(self, start);
                crate::ast::visit_mut_expr(self, end);
                crate::ast::visit_mut_block(self, body);
                self.layers.pop();
                if let Some(block) = else_block {
                    crate::ast::visit_mut_block(self, block);
                }
            }
            _ => crate::ast::visit_mut_expr(self, expr),
            }
        }
        fn visit_mut_block(&mut self, block: &mut crate::ast::expr::Block) {
            self.layers.push(ContextLayer::default());
            crate::ast::visit_mut_block(self, block);
            self.layers.pop();
        }
    }
}
