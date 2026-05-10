use crate::ast::path::ValueBinding;
use crate::ast::path::TypeBinding;
use crate::ast::path::AbsolutePath;
use std::collections::HashMap;
use crate::INDENT;

#[derive(Default)]
struct LookupCache {
    //type_aliases: HashMap<AbsolutePath, crate::ast::Type>,
    modules: HashMap<AbsolutePath, ModuleIndex>,
}
struct ModuleIndex {
    path: AbsolutePath,
    types: HashMap<crate::Ident, TypeEnt>,
    values: HashMap<crate::Ident, ValueBinding>,
}
#[derive(Debug)]
enum VariantInfo {
    Value,
    Type,
}
#[derive(Debug)]
enum TypeEnt {
    Module(AbsolutePath),
    Enum(AbsolutePath, bool, HashMap<crate::Ident, (usize, VariantInfo,)>),
    Type(TypeBinding),
}
impl From<TypeBinding> for TypeEnt {
    fn from(value: TypeBinding) -> Self {
        TypeEnt::Type(value)
    }
}

pub fn resolve(ast_crate: &mut crate::ast::Crate)
{
    println!("resolve");

    let mut lc = LookupCache::default();
    fill_index(&mut lc, &ast_crate.module, AbsolutePath::new_current());
    for (name, m) in &ast_crate.externals {
        fill_index(&mut lc, m, AbsolutePath::new_extern(name.clone()));
    }

    resolve_mod(&lc, &mut ast_crate.module, AbsolutePath::new_current());

    resolve_type_aliases(ast_crate);
}

/// Fill the lookup cache for a module
fn fill_index(lc: &mut LookupCache, module: &crate::ast::items::Module, path: AbsolutePath)
{
    let mut mod_index = ModuleIndex {
        path: path.clone(),
        types: Default::default(),
        values: Default::default(),
    };
    for v in &module.items {
        use crate::ast::items::ItemType;
        match &v.ty {
        ItemType::Use(_) => todo!("Handle `use`"),
        ItemType::ExternCrate(lib_name) => {
            let name = v.name.clone().unwrap();
            let p = AbsolutePath::new_extern(lib_name.clone());
            mod_index.types.insert(name, TypeEnt::Module(p.clone()));
        }
        ItemType::Module(module) => {
            let name = v.name.clone().unwrap();
            let p = path.append(name.clone());
            mod_index.types.insert(name, TypeEnt::Module(p.clone()));
            fill_index(lc, module, p)
        },
        ItemType::TypeAlias(_) => {
            let name = v.name.clone().unwrap();
            mod_index.types.insert(name.clone(), TypeBinding::Alias(path.append(name)).into());
        },
        
        ItemType::ExternBlock(eb) => {
            for i in &eb.items {
                match i.ty {
                crate::ast::items::ExternItemType::Function(_) => {
                    mod_index.values.insert(i.name.clone(), ValueBinding::Function(path.append(i.name.clone())));
                },
                crate::ast::items::ExternItemType::Static(_) => {
                    mod_index.values.insert(i.name.clone(), ValueBinding::Static(path.append(i.name.clone())));
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
            let p = path.append(name.clone());
            mod_index.types.insert(name, TypeEnt::Enum(p, had_data, vi));
        },
        ItemType::Struct(_) => {
            let name = v.name.clone().unwrap();
            mod_index.types.insert(name.clone(), TypeBinding::Struct(path.append(name)).into());
        },
        ItemType::Union(_) => {
            let name = v.name.clone().unwrap();
            mod_index.types.insert(name.clone(), TypeBinding::Union(path.append(name)).into());
        },
        ItemType::Function(_) => {
            let name = v.name.clone().unwrap();
            mod_index.values.insert(name.clone(), ValueBinding::Function(path.append(name)));
        },
        ItemType::Static(_) => {
            let name = v.name.clone().unwrap();
            mod_index.values.insert(name.clone(), ValueBinding::Static(path.append(name)));
        },
        ItemType::Constant(_) => {
            let name = v.name.clone().unwrap();
            mod_index.values.insert(name.clone(), ValueBinding::Constant(path.append(name)));
        },
        }
    }
    println!("INDEX {path}: V={:?} T={:?}", mod_index.values, mod_index.types);
    lc.modules.insert(path, mod_index);
}

fn resolve_mod(lc: &LookupCache, module: &mut crate::ast::items::Module, path: AbsolutePath)
{
    let _i = INDENT.inc("resolve_mod");
    println!("{INDENT}resolve_mod: {} items", module.items.len());

    // Make a list of items for use by inner resolve
    let item_scope = lc.modules.get(&path).expect("Module index not populated");

    for v in &mut module.items {
        use crate::ast::items::ItemType;
        let mut cx = Context::new(lc, &item_scope);
        match &mut v.ty {
        ItemType::Module(module) => resolve_mod(lc, module, path.append(v.name.clone().unwrap())),
        ItemType::Use(_) => {},
        ItemType::ExternCrate(_) => {},
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
                    resolve_const_expr(lc, &item_scope, expr_root, &mut [])
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
            resolve_expr(lc, &item_scope, &mut function.code, &mut function.sig.args);
        },
        ItemType::Static(s) => {
            println!("{INDENT}resolve_mod: Static {}", v.name.as_ref().unwrap());
            cx.resolve_type(&mut s.ty);
            resolve_const_expr(lc, &item_scope, &mut s.value, &mut []);
        },
        ItemType::Constant(i) => {
            cx.resolve_type(&mut i.ty);
            resolve_const_expr(lc, &item_scope, &mut i.value, &mut []);
        },
        }
    }
}

/// Expand type aliases to the inner type
fn resolve_type_aliases(ast_crate: &mut crate::ast::Crate)
{
    let mut s = State { aliases: Default::default() };
    s.enum_from(&ast_crate.module, AbsolutePath::new_current());
    s.resolve_in(&mut ast_crate.module);

    use crate::ast::items::ItemType;
    struct State {
        aliases: HashMap<AbsolutePath, crate::ast::Type>,
    }
    impl State {
        fn enum_from(&mut self, module: &crate::ast::items::Module, mp: AbsolutePath) {
            for v in &module.items {
                match &v.ty {
                ItemType::Module(m) => self.enum_from(m, mp.append(v.name.clone().unwrap())),
                ItemType::TypeAlias(t) => {
                    self.aliases.insert(mp.append(v.name.clone().unwrap()), t.clone());
                },
                _ => {},
                }
            }
        }
        fn resolve_in(&self, module: &mut crate::ast::items::Module) {
            for v in &mut module.items {
                match &mut v.ty {
                ItemType::Module(m) => self.resolve_in(m),
                ItemType::TypeAlias(t) => {
                    self.resolve_in_ty(t);
                },
                _ => {},
                }
            }
        }
        fn resolve_in_ty(&self, ty: &mut crate::ast::Type) {
            use crate::ast::ty::TypeKind;
            match &mut ty.kind {
            TypeKind::Infer { .. }
            |TypeKind::Void
            |TypeKind::Bool
            |TypeKind::Integer(_) => {}
            TypeKind::Tuple(items) => {
                for ty in items {
                    self.resolve_in_ty(ty);
                }
            },
            TypeKind::Pointer { is_const: _, inner }
            |TypeKind::UnsizedArray(inner) => self.resolve_in_ty(inner),
            TypeKind::Named(type_path) => {
                let crate::ast::ty::TypePath::Resolved(tb) = type_path else { panic!("Unresolved type") };
                if let TypeBinding::Alias(absolute_path) = tb {
                    *ty = self.aliases[absolute_path].clone();
                    // TODO: Recursion limit
                    self.resolve_in_ty(ty);
                }
            },
            TypeKind::Array { inner, count } => {
                self.resolve_in_ty(inner);
                match count {
                crate::ast::ty::ArraySize::Unevaluated(expr_root) => self.resolve_in_expr(expr_root),
                crate::ast::ty::ArraySize::Known(_) => {},
                }
            },
            TypeKind::TypeOf(expr_in_type) => {
                self.resolve_in_expr(&mut expr_in_type.0);
            },
            }
        }
        fn resolve_in_expr(&self, expr_root: &mut crate::ast::ExprRoot) {
            for ty in &mut expr_root.variables {
                self.resolve_in_ty(ty);
            }
            crate::ast::visit_mut_expr(&mut &*self, &mut expr_root.e);
        }
    }
    impl crate::ast::ExprVisitor for &'_ State {
        fn visit_mut_expr(&mut self, expr: &mut crate::ast::expr::Expr) {
            self.resolve_in_ty(&mut expr.data_ty);
            use crate::ast::expr::ExprKind;
            match &mut expr.kind {
            ExprKind::TypeInfoSizeOf(ty)
            |ExprKind::Cast(_, ty) => self.resolve_in_ty(ty),
            _ => {},
            }
            crate::ast::visit_mut_expr(self, expr)
        }
    
        fn visit_mut_block(&mut self, block: &mut crate::ast::expr::Block) {
            crate::ast::visit_mut_block(self, block)
        }
    }
}

enum PathParent<'a> {
    Module(&'a ModuleIndex),
    Enum(&'a AbsolutePath, bool, &'a HashMap<crate::Ident, (usize, VariantInfo,)>),
}
fn resolve_path_parent<'lc, 'p>(lc: &'lc LookupCache, item_scope: &'lc ModuleIndex, span: &crate::Span, p: &'p crate::ast::Path) -> (PathParent<'lc>, &'p crate::Ident) {
    use crate::ast::path::Root;
    let item_scope = match p.root {
        | Root::None // Look up in the current function then current module
        | Root::Current // current module only, explicit
        => {
            item_scope
        },
        Root::Super(extra_count) => {
            let path = item_scope.path.parent_n(extra_count);
            lc.modules.get(&path).unwrap()
        },
        Root::Root => lc.modules.get(&AbsolutePath::new_current()).unwrap(),
        };
    assert!(p.components.len() > 0);
    let c = p.components.last().expect("Empty path?");
    let mut item_scope = item_scope;
    for i in 0 .. p.components.len() - 1 {
        let n = &p.components[i];
        let Some(t) = item_scope.types.get(n) else {
            panic!("{span}: Unable to find component {n} of path {p:?}", span=span)
        };
        match t {
        TypeEnt::Module(absolute_path) => {
            let Some(is) = lc.modules.get(absolute_path) else { panic!("Module {} has no index", absolute_path); };
            item_scope = is;
        },
        TypeEnt::Enum(ap, is_data, variants) => {
            if i != p.components.len() - 2 {
                panic!("{span}: Getting child item of invalid type (enum variant)", span=span);
            }
            return (PathParent::Enum(&ap, *is_data, &variants), c,);
        },
        TypeEnt::Type(_) => panic!("{span}: Getting child item of invalid type", span=span)
        }
    }
    (PathParent::Module(item_scope), c)
}

fn resolve_path_type(lc: &LookupCache, item_scope: &ModuleIndex, span: &crate::Span, p: &crate::ast::Path) -> TypeBinding {
    let (par, c) = resolve_path_parent(lc, item_scope, span, p);
    match par {
    PathParent::Module(item_scope) => {
        let Some(t) = item_scope.types.get(c) else {
            panic!("{span}: Unable to find component {c} of path {p:?} (in {mp})", p=p, mp=item_scope.path);
        };
        match t
        {
        TypeEnt::Module(_) => todo!(),
        TypeEnt::Enum(absolute_path, is_data, _) => if *is_data {
            TypeBinding::DataEnum(absolute_path.clone())
        }
        else {
            TypeBinding::ValueEnum(absolute_path.clone())
        },
        TypeEnt::Type(type_binding) => type_binding.clone(),
        }
        },
    PathParent::Enum(ap, _is_data, variants) => {
        if let Some((idx, VariantInfo::Type)) = variants.get(c) {
            return TypeBinding::EnumVariant(ap.append(c.clone()), *idx)
        }
        panic!("{span}: Unable to find component {c} of path {p:?}", p=p)
        },
    }
}

struct Context<'a> {
    lc: &'a LookupCache,
    item_scope: &'a ModuleIndex,
    next_index: u32,
    layers: Vec<ContextLayer>,
}
#[derive(Default)]
struct ContextLayer {
    names: ::std::collections::HashMap<crate::Ident, u32>,
}
impl<'a> Context<'a> {
    fn new(lc: &'a LookupCache, item_scope: &'a ModuleIndex) -> Self {
        Context {
            lc,
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
                let r = resolve_path_type(self.lc, self.item_scope, &ty.span, raw_path);
                *path = crate::ast::ty::TypePath::Resolved(r);
            }
        },
        TypeKind::Pointer { is_const: _, inner } => {
            self.resolve_type(inner);
        },
        TypeKind::Array { inner, count } => {
            self.resolve_type(inner);
            match count {
            crate::ast::ty::ArraySize::Unevaluated(expr_root) => resolve_expr(self.lc, self.item_scope, expr_root, &mut []),
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

    fn resolve_path_value(&self, span: &crate::parser::lex::Span, p: &mut crate::ast::Path) -> ValueBinding {
        // Handle variables first
        if let crate::ast::path::Root::None = p.root {
            if p.components.len() == 1 {
                let c = p.components.first().expect("Empty path?");
                for layer in self.layers.iter().rev() {
                    if let Some(v) = layer.names.get(c) {
                        return ValueBinding::Local(*v);
                    }
                }
            }
        }

        let (par, c) = resolve_path_parent(self.lc, self.item_scope, span, p);
        match par {
        PathParent::Module(item_scope) => {
            let Some(v) = item_scope.values.get(c) else {
                panic!("{span}: Unable to find component {c} of path {p:?} (searching in {pp})", p=p, pp=item_scope.path);
            };
            return v.clone();
        },
        PathParent::Enum(ap, is_data, variants) => {
            let Some((idx, VariantInfo::Value)) = variants.get(c) else {
                panic!("{span}: Unable to find component {c} of path {p:?}", p=p);
            };
            return match is_data {
            true => ValueBinding::DataEnumVariant(ap.append(c.clone()), *idx),
            false => ValueBinding::ValueEnumVariant(ap.append(c.clone()), *idx),
            };
        },
        }
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
            *binding = Some(resolve_path_type(self.lc, self.item_scope, &expr.span, p));
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

fn resolve_const_expr(lc: &LookupCache, item_scope: &ModuleIndex, expr: &mut crate::ast::items::ConstantValue, args: &mut [(crate::ast::Pattern, crate::ast::Type)])
{
    match expr {
    crate::ast::items::ConstantValue::Unknown(expr_root) => resolve_expr(lc, item_scope, expr_root, args),
    crate::ast::items::ConstantValue::Evaluated(items) => {},
    }
}

fn resolve_expr(lc: &LookupCache, item_scope: &ModuleIndex, expr: &mut crate::ast::ExprRoot, args: &mut [(crate::ast::Pattern, crate::ast::Type)])
{
    let _i = INDENT.inc("resolve_expr");
    let mut c = Context::new(lc, item_scope);
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
