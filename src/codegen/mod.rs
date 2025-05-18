
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