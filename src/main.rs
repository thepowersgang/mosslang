
use proc_macro2::Ident;

mod ast;
mod parser;
mod expand;
mod codegen;

#[derive(::gumdrop::Options)]
struct Options
{
    help: bool,

    #[options(free, required)]
    root_file: ::std::path::PathBuf,

    #[options(short="o"/*, required*/)]
    output: Option<::std::path::PathBuf>,
}

fn main() {
    let args: Options = ::gumdrop::Options::parse_args_default_or_exit();

    // 1. Parse
    let mut ast_crate = {
        let parsed = parser::parse_file(&args.root_file).unwrap();
        ast::Crate {
            module: parsed.module,
            attributes: parsed.self_attrs,
            }
        };

    // 2. Expand
    expand::expand_crate(&mut ast_crate);

    // 3. Flatten/simplify
    //let hir_crate = hir::from_ast(ast_crate);
    // 4. Typecheck
    //typecheck::check_crate(&mut hir_crate);
    // 5. Translate
    //codegen::generate(&args.output, ast_crate)
}
