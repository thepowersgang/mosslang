
use parser::lex::{Ident,Span};

mod ast;
mod parser;
mod expand;
mod typecheck;
mod resolve;
mod codegen;
mod helpers;

#[derive(::gumdrop::Options)]
struct Options
{
    help: bool,

    #[options(free, required)]
    root_file: ::std::path::PathBuf,

    #[options(short="o"/*, required*/)]
    output: Option<::std::path::PathBuf>,
}
#[cfg(target_os="linux")]
const ISA_NAME: &str = "x86_64-elf";
#[cfg(target_os="windows")]
const ISA_NAME: &str = "x86_64-windows";

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

    // 2. Expand (apply macros and conditional compilation)
    expand::expand_crate(&mut ast_crate);

    resolve::resolve(&mut ast_crate);

    // 3. Flatten/simplify
    //let hir_crate = hir::from_ast(ast_crate);
    // 4. Typecheck and populate
    typecheck::typecheck(&mut ast_crate);
    // 5. Generate output
    let output_path = match args.output {
        Some(p) => p,
        None => {
            let mut p = args.root_file.clone();
            if cfg!(target_os="windows") {
                p.set_extension("obj");
            }
            else {
                p.set_extension("o");
            }
            p
            },
        };
    match codegen::generate(&output_path, ISA_NAME, ast_crate) {
    Ok( () ) => {}
    Err(e) => todo!("Handle error from codegen: {:?}", e),
    }

    //::std::env::set_var("OPT_LEVEL", "1");
    //let c = cc::Build::new()
    //    .object(output_path)
    //    .get_compiler()
    //    .to_command();
    //eprintln!("{:?}", c);
}

mod indent {
    use ::std::sync::atomic::{AtomicUsize, Ordering};
    pub struct Indent {
        v: ::std::sync::atomic::AtomicUsize,
    }
    impl Indent {
        pub const fn new() -> Self {
            Indent { v: AtomicUsize::new(0) }
        }
        pub fn inc<'a>(&'a self, name: &'a str) -> Inc<'a> {
            println!("{}> {}", self, name);
            self.v.fetch_add(1, Ordering::Relaxed);
            Inc(self, name)
        }
        pub fn inc_f<'a>(&'a self, name: &'a str, a: ::core::fmt::Arguments) -> Inc<'a> {
            println!("{}> {}({})", self, name, a);
            self.v.fetch_add(1, Ordering::Relaxed);
            Inc(self, name)
        }
    }
    impl ::std::fmt::Display for Indent {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            for _ in 0 .. self.v.load(Ordering::Relaxed) {
                f.write_str(" ")?;
            }
            Ok(())
        }
    }
    pub struct Inc<'a>(&'a Indent, &'a str);
    impl ::std::ops::Drop for Inc<'_> {
        fn drop(&mut self) {
            self.0.v.fetch_sub(1, Ordering::Relaxed);
            println!("{}< {}", self.0, self.1);
        }
    }
}
static INDENT: indent::Indent = indent::Indent::new();
