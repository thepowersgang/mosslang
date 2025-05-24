
// TODO: What sort of backend? Simple assembly? an IR?
// Cranelift probably
use crate::INDENT;
use crate::ast::path::AbsolutePath;
use ::std::collections::HashMap;

mod type_info;
mod ir;

#[cfg(feature="cranelift")]
mod backend_cranelift;

struct State<'a> {
    ofp: ::std::fs::File,
    constants: HashMap<AbsolutePath,&'a crate::ast::ExprRoot>,
    fields: HashMap<AbsolutePath,HashMap<crate::Ident, (usize, crate::ast::Type)>>,
    types_cache: ::std::cell::RefCell< ::std::collections::BTreeMap< crate::ast::Type, type_info::TypeInfoRef > >,
}

pub fn generate(output: &::std::path::Path, krate: crate::ast::Crate) -> Result<(),::std::io::Error>
{
    let mut state = State {
        ofp: ::std::fs::File::create(output)?,
        constants: Default::default(),
        fields: Default::default(),
        types_cache: Default::default(),
    };
    fn enum_module<'a>(state: &mut State<'a>, module: &'a crate::ast::items::Module, path: AbsolutePath) {
        for item in &module.items {
            use crate::ast::items::ItemType;
            match item.ty {
            ItemType::Constant(ref v) => {
                state.constants.insert(path.append(item.name.clone().unwrap()), &v.value);
            },
            ItemType::Struct(ref s) => {
                let ap = path.append(item.name.as_ref().unwrap().clone());
                let fields = s.fields.iter().enumerate()
                    .map(|(i,v)| (v.name.clone(), (i, v.ty.clone())))
                    .collect();
                state.fields.insert( ap.clone(), fields );
            },
            _ => {},
            }
        }
    }
    enum_module(&mut state, &krate.module, AbsolutePath(Vec::new()));
    state.visit_module(&krate.module);
    Ok( () )
}

impl<'a> State<'a> {
    fn type_info(&self, ty: &crate::ast::Type) -> type_info::TypeInfoRef {
        use self::type_info::TypeInfo;
        use crate::ast::ty::{TypeKind,IntClass};
        
        if let Some(v) = self.types_cache.borrow().get(ty) {
            return v.clone();
        }
        // TODO: Handle infinite recursion by pushing to a stack and popping after `new` is created

        let new = ::std::rc::Rc::new(match &ty.kind {
            TypeKind::Infer { .. } => panic!("Found IVar during codegen? {:?}", ty),
            TypeKind::TypeOf(..) => panic!("Found TypeOf during codegen? {:?}", ty),
            TypeKind::Void => panic!("Type info for `void`?"),
            TypeKind::Bool => TypeInfo::make_primitive(IntClass::Unsigned(0)),
            TypeKind::Integer(int_class) => TypeInfo::make_primitive(int_class.clone()),
            TypeKind::Tuple(items) => {
                let mut repr_fields = Vec::with_capacity(items.len());
                let mut size = 0;
                let mut align = 0;
                for ity in items {
                    let i = self.type_info(ity);
                    size += (i.align() - size % i.align()) % i.align();
                    align = align.max(i.align());
                    let o = size;
                    size += i.size();
                    repr_fields.push((o, i));
                }
                TypeInfo::make_composite(size, align, repr_fields)
            },
            TypeKind::Named(path) => {
                let crate::ast::ty::TypePath::Resolved(type_binding) = path else { panic!("Unbound type during codegen. {:?}", ty); };
                use crate::ast::path::TypeBinding;
                match type_binding {
                TypeBinding::Alias(_) => panic!("Unresolved type alias during codegen. {:?}", ty),
                TypeBinding::EnumVariant(_, _) => panic!("Type resolved to EnumVariant, only valid in patterns"),
                TypeBinding::Union(absolute_path) => todo!(),
                TypeBinding::Struct(absolute_path) => {
                    let Some(f) = self.fields.get(absolute_path) else { panic!("struct missing from fields map: {:?}", ty) };
                    let mut fields: Vec<_> = f.values().collect();
                    fields.sort();
                    let mut repr_fields = Vec::with_capacity(fields.len());
                    let mut size = 0;
                    let mut align = 0;
                    for (_, ity) in fields {
                        let i = self.type_info(ity);
                        size += (i.align() - size % i.align()) % i.align();
                        align = align.max(i.align());
                        let o = size;
                        size += i.size();
                        repr_fields.push((o, i));
                    }
                    TypeInfo::make_composite(size, align, repr_fields)
                },
                TypeBinding::ValueEnum(absolute_path) => todo!(),
                TypeBinding::DataEnum(absolute_path) => todo!(),
                }
            },
            TypeKind::Pointer { .. } => TypeInfo::make_primitive(IntClass::PtrInt),
            TypeKind::Array { inner, count } => todo!(),
            TypeKind::UnsizedArray(inner) => TypeInfo::make_array(0, self.type_info(inner)),
            });
        self.types_cache.borrow_mut().entry(ty.clone()).or_insert(new).clone()
    }
}

impl<'a> State<'a> {
    fn visit_module(&mut self, module: &crate::ast::items::Module) {
        for item in &module.items {
            use crate::ast::items::ItemType;
            match &item.ty {
            | ItemType::ExternBlock(_)
            | ItemType::TypeAlias(_)
            | ItemType::Struct(_)
            | ItemType::Enum(_)
            | ItemType::Union(_)
            | ItemType::Constant(_)
                => {},

            ItemType::Function(fcn) => self.emit_function(item.name.as_ref().unwrap(), fcn),
            ItemType::Static(s) => self.emit_static(item.name.as_ref().unwrap(), s),
            }
        }
    }

    fn emit_function(&mut self, name: &super::Ident, f: &crate::ast::items::Function) {
        let _i = INDENT.inc("emit_function");
        println!("{INDENT}emit_function: {name}");
        let ir = ir::from_expr(self, &f.code);

        use ::std::io::Write;
        write!(self.ofp, "fn {name}() {{\n").unwrap();
        ir::dump(&mut IndentFile(&mut &self.ofp, true), &ir).unwrap();
        write!(self.ofp, "}}\n\n").unwrap();
    }
    fn emit_static(&mut self, name: &super::Ident, s: &crate::ast::items::Static) {
        let _i = INDENT.inc("emit_static");
        println!("{INDENT}emit_static: {name}");
        let ir = ir::from_expr(self, &s.value);
        
        use ::std::io::Write;
        write!(self.ofp, "static {name}: _ = {{\n").unwrap();
        ir::dump(&mut IndentFile(&mut self.ofp, true), &ir).unwrap();
        write!(self.ofp, "}};\n\n").unwrap();
    }
}

struct IndentFile<F>(F, bool);
impl<F> IndentFile<F>
where
    F: ::std::io::Write
{
    fn write_seg(&mut self, rv: &mut usize, buf: &[u8]) -> Option<std::io::Result<usize>> {
        if ::std::mem::replace(&mut self.1, false) {
            match self.0.write(b"    ") {
            Err(e) => return Some(Err(e)),
            Ok(_) => {},
            }
        }
        match self.0.write(buf) {
        Err(e) => return Some(Err(e)),
        Ok(v) => {
            *rv += v;
            if v != buf.len() {
                return Some(Ok(v))
            }
        }
        }
        None
    }
}
impl<F> ::std::io::Write for IndentFile<F>
where
    F: ::std::io::Write
{
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut it = buf.split(|&v| v == b'\n');
        let mut cur = it.next().unwrap();

        let mut rv = 0;

        for v in it {
            if let Some(v) = self.write_seg(&mut rv, cur) {
                return v;
            }
            if let Some(v) = self.write_seg(&mut rv, b"\n") {
                return v;
            }
            // This is only reached if the buffer contained at least one newline
            self.1 = true;
            cur = v;
        }

        if cur.len() == 0 {
            self.1 = true;
        }
        else {
            if let Some(v) = self.write_seg(&mut rv, cur) {
                return v;
            }
        }
        assert!(rv <= buf.len());
        Ok(rv)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}