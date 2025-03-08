
// TODO: What sort of backend? Simple assembly? an IR?
// Cranelift probably
use crate::INDENT;

mod ir;

struct State {
    ofp: ::std::fs::File,
}

pub fn generate(output: &::std::path::Path, krate: crate::ast::Crate) -> Result<(),::std::io::Error>
{
    let mut state = State {
        ofp: ::std::fs::File::create(output)?,
    };
    state.visit_module(&krate.module);
    Ok( () )
}

impl State {
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
    }
    fn emit_static(&mut self, name: &super::Ident, s: &crate::ast::items::Static) {
    }
}