use crate::ast::path::ValueBinding;
use crate::ast::path::TypeBinding;
use crate::ast::path::AbsolutePath;
use std::collections::HashMap;
use crate::INDENT;

#[derive(Default)]
struct LookupCache {
    type_aliases: HashMap<AbsolutePath, crate::ast::Type>,
}

pub fn resolve(ast_crate: &mut crate::ast::Crate)
{
    println!("resolve");

    // Type aliases
    let mut lc = LookupCache::default();
    fill_lc(&mut lc, &ast_crate.module, AbsolutePath(Vec::new()));

    resolve_mod(&lc, &mut ast_crate.module)
}

fn fill_lc(lc: &mut LookupCache, module: &crate::ast::items::Module, path: AbsolutePath)
{
    for i in &module.items {
        use crate::ast::items::ItemType;
        match &i.ty {
        //ItemType::Module(module) => fill_lc(lc, module),
        ItemType::TypeAlias(ty) => {
            lc.type_aliases.insert(path.append(i.name.as_ref().unwrap().clone()), ty.clone());
        },
        _ => {},
        }
    }
}

fn resolve_mod(lc: &LookupCache, module: &mut crate::ast::items::Module)
{
    let _i = INDENT.inc("resolve_mod");
    println!("{INDENT}resolve_mod: {} items", module.items.len());

    // Make a list of items for use by inner resolve
    let item_scope = {
        let get_ap = |name| {
            AbsolutePath(vec![name])
        };
        let mut item_scope = ItemScope {
            lc,
            types: Default::default(),
            values: Default::default(),
        };
        for v in &module.items {
            use crate::ast::items::ItemType;
            match &v.ty {
            //ItemType::Module(module) => resolve_mod(lc, module),
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
                let mut had_data = false;
                let vi = enm.variants.iter().enumerate().map(|(i, v)| (v.name.clone(), (i, match v.ty {
                    crate::ast::items::EnumVariantTy::Bare => VariantInfo::Value,
                    crate::ast::items::EnumVariantTy::Value(_) => VariantInfo::Value,
                    crate::ast::items::EnumVariantTy::Data(_) => { had_data = true; VariantInfo::Type },
                    },),)).collect();
                let tb = if had_data {
                    TypeBinding::DataEnum(get_ap(name.clone()))
                }
                else {
                    TypeBinding::ValueEnum(get_ap(name.clone()))
                };
                item_scope.types.insert(name, (tb, Some(vi)));
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
        let mut cx = Context::new(&item_scope);
        match &mut v.ty {
        //crate::ast::items::ItemType::Module(module) => expand_module(module),
        ItemType::ExternBlock(eb) => {
            for v in &mut eb.items {
                match &mut v.ty {
                crate::ast::items::ExternItemType::Function(function_signature) => {
                    for a in &mut function_signature.args {
                        cx.resolve_type(&mut a.1);
                    }
                    cx.resolve_type(&mut function_signature.ret);
                },
                crate::ast::items::ExternItemType::Static(extern_static) => {
                    cx.resolve_type(&mut extern_static.ty);
                },
                }
            }
        },
        ItemType::TypeAlias(ty) => {
            cx.resolve_type(ty);
        },
        ItemType::Struct(str) => {
            for f in &mut str.fields {
                cx.resolve_type(&mut f.ty);
            }
        },
        ItemType::Enum(enm) => {
            for v in &mut enm.variants {
                match &mut v.ty {
                crate::ast::items::EnumVariantTy::Bare => {},
                crate::ast::items::EnumVariantTy::Value(expr_root) => {
                    resolve_expr(&item_scope, expr_root, &mut [])
                    },
                crate::ast::items::EnumVariantTy::Data(_) => todo!(),
                }
            }
        },
        ItemType::Union(_) => {
        },
        ItemType::Function(function) => {
            println!("{INDENT}resolve_mod: Function {}", v.name.as_ref().unwrap());
            for a in &mut function.sig.args {
                cx.resolve_type(&mut a.1);
            }
            cx.resolve_type(&mut function.sig.ret);
            resolve_expr(&item_scope, &mut function.code, &mut function.sig.args);
        },
        ItemType::Static(s) => {
            println!("{INDENT}resolve_mod: Static {}", v.name.as_ref().unwrap());
            cx.resolve_type(&mut s.ty);
            resolve_expr(&item_scope, &mut s.value, &mut []);
        },
        ItemType::Constant(i) => {
            cx.resolve_type(&mut i.ty);
            resolve_expr(&item_scope, &mut i.value, &mut []);
        },
        }
    }
}

enum VariantInfo {
    Value,
    Type,
}
struct ItemScope<'a> {
    lc: &'a LookupCache, 
    types: HashMap<crate::Ident, (crate::ast::path::TypeBinding, Option<HashMap<crate::Ident, (usize, VariantInfo,)>>,)>,
    values: HashMap<crate::Ident, crate::ast::path::ValueBinding>,
}

fn resolve_path_type(item_scope: &ItemScope, span: &crate::Span, p: &crate::ast::Path) -> crate::ast::path::TypeBinding {
    assert!(matches!(p.root, crate::ast::path::Root::None));
    let c = p.components.first().expect("Empty path?");
    if p.components.len() == 2 {
        let vn = &p.components[1];
        if let Some((ap, Some(variants))) = item_scope.types.get(c) {
            if let Some((idx, VariantInfo::Value)) = variants.get(vn) {
                let (TypeBinding::DataEnum(ap)|TypeBinding::ValueEnum(ap)) = ap else { unreachable!() };
                return crate::ast::path::TypeBinding::EnumVariant(ap.append(vn.clone()), *idx)
            }
        }
    }
    if p.components.len() == 1 {
        if let Some((v,_)) = item_scope.types.get(c) {
            return v.clone();
        }
    }
    todo!("{} Resolve type path {:?}", span, p);
}

struct Context<'a> {
    item_scope: &'a ItemScope<'a>,
    next_index: u32,
    layers: Vec<ContextLayer>,
}
#[derive(Default)]
struct ContextLayer {
    names: ::std::collections::HashMap<crate::Ident, u32>,
}
impl<'a> Context<'a> {
    fn new(item_scope: &'a ItemScope) -> Self {
        Context {
            item_scope,
            next_index: 0,
            layers: Vec::new(),
        }
    }
    fn define_var(&mut self, name: &crate::Ident) -> u32 {
        if let Some(l) = self.layers.last_mut() {
            let rv = self.next_index;
            l.names.insert(name.clone(), rv);
            self.next_index += 1;
            rv
        }
        else {
            todo!("Defining variable with no layers?")
        }
    }
    

    fn resolve_type(&mut self, ty: &mut crate::ast::Type) {
        use crate::ast::ty::TypeKind;
        use crate::ast::ExprVisitor;
        match &mut ty.kind {
        TypeKind::Infer { .. } => {},
        TypeKind::Integer(..) => {},
        TypeKind::Void => {},
        TypeKind::Bool => {},
        TypeKind::Tuple(items) => {
            for ty in items {
                self.resolve_type(ty);
            }
        },
        TypeKind::Named(ref mut path) => {
            if let crate::ast::ty::TypePath::Unresolved(raw_path) = path {
                let r = resolve_path_type(self.item_scope, &ty.span, raw_path);
                if let TypeBinding::Alias(t) = r {
                    let a = self.item_scope.lc.type_aliases.get(&t).expect("Missing TypeAlias");
                    *ty = a.clone();
                    // Recurse
                    self.resolve_type(ty);
                }
                else {
                    *path = crate::ast::ty::TypePath::Resolved(r);
                }
            }
        },
        TypeKind::Pointer { is_const: _, inner } => {
            self.resolve_type(inner);
        },
        TypeKind::Array { inner, count } => {
            self.resolve_type(inner);
            match count {
            crate::ast::ty::ArraySize::Unevaluated(expr_root) => resolve_expr(self.item_scope, expr_root, &mut []),
            crate::ast::ty::ArraySize::Known(_) => {},
            }
        },
        TypeKind::UnsizedArray(inner) => {
            self.resolve_type(inner);
        },
        TypeKind::TypeOf(expr) => {
            self.visit_mut_expr(&mut expr.0.e);
        }
        }
    }

    fn resolve_path_value(&self, span: &crate::parser::lex::Span, p: &mut crate::ast::Path) -> crate::ast::path::ValueBinding {
        assert!(matches!(p.root, crate::ast::path::Root::None));
        let c = p.components.first().expect("Empty path?");
        if p.components.len() == 2 {
            let vn = &p.components[1];
            if let Some((ap, Some(variants))) = self.item_scope.types.get(c) {
                if let Some((idx, VariantInfo::Value)) = variants.get(vn) {
                    return match ap {
                    TypeBinding::DataEnum(ap) => crate::ast::path::ValueBinding::DataEnumVariant(ap.append(vn.clone()), *idx),
                    TypeBinding::ValueEnum(ap) => crate::ast::path::ValueBinding::ValueEnumVariant(ap.append(vn.clone()), *idx),
                    _ => unreachable!(),
                    };
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
        todo!("{}Resolve value path {:?}", span, p);
    }
    fn visit_mut_pattern_value(&mut self, span: &crate::parser::lex::Span, v: &mut crate::ast::pattern::Value) {
        use crate::ast::pattern::{Value,NamedValue};
        match v {
        Value::Integer(_) => {},
        Value::NamedValue(NamedValue::Unbound(path)) => {
            *v = Value::NamedValue(value_binding_to_named_pattern(span, self.resolve_path_value(span, path)));
        },
        Value::NamedValue(_) => {},
        }
    }
}
fn value_binding_to_named_pattern(span: &crate::parser::lex::Span, vb: ValueBinding) -> crate::ast::pattern::NamedValue {
    use crate::ast::pattern::NamedValue;
    match vb
    {
    ValueBinding::Local(_) => todo!(),
    ValueBinding::Function(_) => panic!("{span}: Using a function as a match value? {:?}", vb),
    ValueBinding::Static(_) => todo!(),
    ValueBinding::Constant(absolute_path) => NamedValue::Constant(absolute_path),
    ValueBinding::DataEnumVariant(absolute_path, idx)
    |ValueBinding::ValueEnumVariant(absolute_path, idx) => NamedValue::EnumVariant(absolute_path, idx),
    }
}
impl crate::ast::ExprVisitor for Context<'_> {
    fn visit_mut_pattern(&mut self, p: &mut crate::ast::Pattern, refutable: bool) {
        println!("{INDENT}visit_mut_pattern: {:?}", p);
        for b in &mut p.bindings {
            b.index = Some(self.define_var(&b.name));
        }
        use crate::ast::pattern::PatternTy;
        match &mut p.ty {
        PatternTy::Any => {},
        PatternTy::MaybeBind(ident) => {
            if refutable {
                // TODO: look up globals
                if let Some(v) = self.item_scope.values.get(ident) {
                    p.ty = PatternTy::ValueSingle(crate::ast::pattern::Value::NamedValue(
                        value_binding_to_named_pattern(&p.span, v.clone())
                    ));
                    return ;
                }
            }
            p.bindings.push(crate::ast::pattern::PatternBinding {
                name: ident.clone(),
                index: Some(self.define_var(ident)),
                });
            p.ty = PatternTy::Any;
        },
        PatternTy::Multiple(patterns) => {
            for p in patterns {
                self.visit_mut_pattern(p, refutable);
            }
        },
        PatternTy::ValueSingle(v) => {
            self.visit_mut_pattern_value(&p.span, v);
        },
        PatternTy::ValueRangeExcl(v1, v2)|PatternTy::ValueRangeIncl(v1, v2) => {
            self.visit_mut_pattern_value(&p.span, v1);
            self.visit_mut_pattern_value(&p.span, v2);
        },
        PatternTy::Tuple(patterns) => {
            for p in patterns {
                self.visit_mut_pattern(p, refutable);
            }
        },
        }
    }
    fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
        use crate::ast::expr::ExprKind;
        let expr_p: *const _ = expr;
        match &mut expr.kind {
        ExprKind::CallPath(p, binding, ..) => {
            *binding = Some(self.resolve_path_value(&expr.span, p));
            },
        ExprKind::NamedValue(p, binding) => {
            *binding = Some(self.resolve_path_value(&expr.span, p));
            println!("{INDENT}NamedValue: {:?} = {:?} @ {:p}", p, binding.as_ref().unwrap(), expr_p);
            },
        ExprKind::Struct(p, binding, _) => {
            *binding = Some(resolve_path_type(self.item_scope, &expr.span, p));
            println!("{INDENT}Struct: {:?} = {:?} @ {:p}", p, binding.as_ref().unwrap(), expr_p);
            },
        ExprKind::Cast(_, ty) => self.resolve_type(ty),
        ExprKind::TypeInfoSizeOf(ty) => self.resolve_type(ty),
        _ => {},
        }
        // Special case for blocks that generate scopes without being blocks
        match &mut expr.kind {
        ExprKind::Match { value, branches } => {
            self.visit_mut_expr(value);
            for a in branches {
                self.layers.push(ContextLayer::default());
                self.visit_mut_pattern(&mut a.pat, true);
                self.visit_mut_expr(&mut a.val);
                self.layers.pop();
            }
        }
        ExprKind::ForLoop { pattern, start, end, body, else_block } => {
            self.layers.push(ContextLayer::default());
            self.visit_mut_pattern(pattern, false);
            self.visit_mut_expr(start);
            self.visit_mut_expr(end);
            self.visit_mut_block(body);
            self.layers.pop();
            if let Some(block) = else_block {
                self.visit_mut_block(block);
            }
        }
        _ => crate::ast::visit_mut_expr(self, expr),
        }
    }
    fn visit_mut_block(&mut self, block: &mut crate::ast::expr::Block) {
        self.layers.push(ContextLayer::default());
        for s in &mut block.statements {
            match s {
            crate::ast::expr::Statement::Let(_pattern, ty, _expr) => {
                self.resolve_type(ty);
            },
            _ => {},
            }
        }
        crate::ast::visit_mut_block(self, block);
        self.layers.pop();
    }
}

fn resolve_expr(item_scope: &ItemScope, expr: &mut crate::ast::ExprRoot, args: &mut [(crate::ast::Pattern, crate::ast::Type)])
{
    let _i = INDENT.inc("resolve_expr");
    let mut c = Context::new(item_scope);
    // Reserve the non-destructured arguments (implicit first few locals)
    c.next_index = args.len() as _;
    if ! args.is_empty() {
        c.layers.push(Default::default());
        for a in args {
            crate::ast::ExprVisitor::visit_mut_pattern(&mut c, &mut a.0, false);
        }
    }
    crate::ast::visit_mut_expr(&mut c, &mut expr.e);
    expr.variable_count = c.next_index as usize;
}
