
use proc_macro2::Ident;

mod ast;
mod parser;

#[derive(::gumdrop::Options)]
struct Options
{
    help: bool,

    #[options(free, required)]
    root_file: ::std::path::PathBuf,
}

fn main() {
    let args: Options = ::gumdrop::Options::parse_args_default_or_exit();

    // 1. Parse
    let _krate = parser::parse_file(&args.root_file).unwrap();

    // 2. Expand

    // 3. Flatten/simplify
    // 4. Typecheck
    // 5. Translate

    println!("Hello, world!");
}
