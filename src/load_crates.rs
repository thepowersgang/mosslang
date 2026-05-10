pub fn visit_crate(c: &mut crate::ast::Crate)
{
    visit_module(&mut c.externals, &c.module);
}
fn visit_module(dst: &mut ::std::collections::HashMap<crate::Ident,crate::ast::items::Module>, module: &crate::ast::items::Module)
{
    use crate::ast::items::ItemType;
    for i in &module.items {
        match &i.ty {
        ItemType::Module(module) => visit_module(dst, module),
        ItemType::ExternCrate(ident) => {
            if !dst.contains_key(ident) {
                dst.insert(ident.clone(), load_crate(ident));
            }
        },
        _ => {},
        }
    }
}
fn load_crate(name: &crate::Ident) -> crate::ast::items::Module {
    let path = format!("example_code/{}.moss-meta", name);
    match crate::metadata::load_crate(path.as_ref()) {
    Ok(c) => c.module,
    Err(e) => panic!("Failed to load {}: {}", name, e),
    }
}