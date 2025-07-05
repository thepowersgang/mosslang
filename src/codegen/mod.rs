
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
    #[cfg(feature="cranelift")]
    out: backend_cranelift::Context,
    ofp_bare_ir: ::std::fs::File,
    ofp_ssa_ir: ::std::fs::File,
    inner: InnerState<'a>,
}
#[derive(Default)]
struct InnerState<'a> {
    constants: HashMap<AbsolutePath,&'a crate::ast::ExprRoot>,
    statics: HashMap<AbsolutePath,&'a crate::ast::Type>,
    fields: HashMap<AbsolutePath,HashMap<crate::Ident, (usize, &'a crate::ast::Type)>>,
    field_types: HashMap<AbsolutePath,Vec<&'a crate::ast::Type>>,
    types_cache: ::std::cell::RefCell< ::std::collections::BTreeMap< crate::ast::Type, type_info::TypeInfoRef > >,
}

pub fn generate(output: &::std::path::Path, krate: crate::ast::Crate) -> Result<(),::std::io::Error>
{
    let mut state = State {
        #[cfg(feature="cranelift")]
        out: backend_cranelift::Context::new(output, "x86_64-elf"),
        ofp_bare_ir: ::std::fs::File::create(output.with_extension(".moss_ir"))?,
        ofp_ssa_ir: ::std::fs::File::create(output.with_extension(".moss_ssa"))?,
        inner: InnerState::default(),
    };
    fn enum_module<'a>(state: &mut State<'a>, module: &'a crate::ast::items::Module, path: AbsolutePath) {
        for item in &module.items {
            use crate::ast::items::ItemType;
            match item.ty {
            ItemType::ExternBlock(ref b) => {
                for item in &b.items {
                    use crate::ast::items::ExternItemType;
                    match &item.ty {
                    ExternItemType::Function(v) => {
                        #[cfg(feature="cranelift")]
                        state.out.declare_function(&state.inner, path.append(item.name.clone()), v);
                    },
                    ExternItemType::Static(v) => {
                        state.inner.statics.insert(path.append(item.name.clone()), &v.ty);
                        #[cfg(feature="cranelift")]
                        state.out.declare_external_static(&state.inner, path.append(item.name.clone()), &v.ty);
                    },
                    }
                }
            },
            ItemType::Constant(ref v) => {
                state.inner.constants.insert(path.append(item.name.clone().unwrap()), &v.value);
            },
            ItemType::Static(ref v) => {
                state.inner.statics.insert(path.append(item.name.clone().unwrap()), &v.ty);
            },
            ItemType::Struct(ref s) => {
                let ap = path.append(item.name.as_ref().unwrap().clone());
                let fields = s.fields.iter().enumerate()
                    .map(|(i,v)| (v.name.clone(), (i, &v.ty)))
                    .collect();
                state.inner.fields.insert( ap.clone(), fields );
                state.inner.field_types.insert(ap.clone(), s.fields.iter().map(|v| &v.ty).collect());
            },
            ItemType::Function(ref f) => {
                #[cfg(feature="cranelift")]
                state.out.declare_function(&state.inner, path.append(item.name.as_ref().unwrap().clone()), &f.sig);
            }
            _ => {},
            }
        }
    }
    enum_module(&mut state, &krate.module, AbsolutePath(Vec::new()));
    state.visit_module(&krate.module);
    state.out.finalise()?;
    Ok( () )
}

impl<'a> InnerState<'a> {
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
                    let ofs = size;
                    size += i.size();
                    repr_fields.push(type_info::CompositeField {
                        ofs,
                        //orig_ty: ity.clone(),
                        type_info: i
                    });
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
                        let ofs = size;
                        size += i.size();
                        repr_fields.push(type_info::CompositeField {
                            ofs,
                            //orig_ty: (**ity).clone(),
                            type_info: i,
                        });
                    }
                    TypeInfo::make_composite(size, align, repr_fields)
                },
                TypeBinding::ValueEnum(_absolute_path) => {
                    TypeInfo::make_primitive(IntClass::Unsigned(2))
                },
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
    fn type_info(&self, ty: &crate::ast::Type) -> type_info::TypeInfoRef {
        self.inner.type_info(ty)
    }
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
        let ir = ir::Expr::from_ast(self, &f.code, &f.sig.args);

        ir::dump_fcn(&mut self.ofp_bare_ir, name, &f.sig, &ir);
        ir::verify::check(&ir, f.sig.args.len());

        let ssa_ir = {
            println!("{INDENT}emit_function: SSA {name}");
            ir::SsaExpr::new(ir)
        };
        ir::dump_fcn(&mut self.ofp_ssa_ir, name, &f.sig, ssa_ir.get());
        ir::verify::check_ssa(ssa_ir.get(), f.sig.args.len());

        let p = crate::ast::path::AbsolutePath(vec![name.clone()]);
        self.out.lower_function(&self.inner, &p, &ssa_ir);
    }
    fn emit_static(&mut self, name: &super::Ident, s: &crate::ast::items::Static) {
        let _i = INDENT.inc("emit_static");
        println!("{INDENT}emit_static: {name}");
        let ir = ir::Expr::from_ast(self, &s.value, &[]);
        
        ir::dump_static(&mut self.ofp_bare_ir, name, &s.ty, &ir);
        
        let ssa_ir = {
            println!("{INDENT}emit_static: SSA {name}");
            ir::SsaExpr::new(ir)
        };
        ir::dump_static(&mut self.ofp_ssa_ir, name, &s.ty, ssa_ir.get());
    }
}