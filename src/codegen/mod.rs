
// TODO: What sort of backend? Simple assembly? an IR?
// Cranelift probably
use crate::INDENT;
use crate::ast::path::AbsolutePath;
use ::std::collections::HashMap;

mod ir;

#[cfg(feature="cranelift")]
mod backend_cranelift;

struct State<'a> {
    ofp: ::std::fs::File,
    constants: HashMap<AbsolutePath,&'a crate::ast::ExprRoot>,
    fields: HashMap<AbsolutePath,HashMap<crate::Ident, usize>>,
}

pub fn generate(output: &::std::path::Path, krate: crate::ast::Crate) -> Result<(),::std::io::Error>
{
    let mut state = State {
        ofp: ::std::fs::File::create(output)?,
        constants: Default::default(),
        fields: Default::default(),
    };
    fn enum_module<'a>(state: &mut State<'a>, module: &'a crate::ast::items::Module, path: AbsolutePath) {
        for item in &module.items {
            use crate::ast::items::ItemType;
            match item.ty {
            ItemType::Constant(ref v) => {
                state.constants.insert(path.append(item.name.clone().unwrap()), &v.value);
            },
            ItemType::Struct(ref s) => {
                let fields = s.fields.iter().enumerate().map(|(i,v)| (v.name.clone(), i)).collect();
                state.fields.insert(
                    path.append(item.name.as_ref().unwrap().clone()),
                    fields
                );
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
        ir::dump(&mut self.ofp, &ir).unwrap();
    }
    fn emit_static(&mut self, name: &super::Ident, s: &crate::ast::items::Static) {
        let _i = INDENT.inc("emit_static");
        println!("{INDENT}emit_static: {name}");
        let ir = ir::from_expr(self, &s.value);
        ir::dump(&mut self.ofp, &ir).unwrap();
    }
}