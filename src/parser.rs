//! Parse a file into a module tree
//! 

pub mod lex;
mod attrs;
mod expr;
mod items;
mod path;
mod pattern;
mod ty;

use self::attrs::parse_attributes;
use self::expr::{parse_root_expr,parse_root_block};
use self::path::opt_parse_path;
use self::pattern::parse_pattern;
use self::ty::parse_type;

use self::lex::Lexer;

#[derive(Default)]
pub struct ParsedModuleFile {
    pub self_attrs: Vec<crate::ast::Attribute>,
    pub module: crate::ast::items::Module,
}

type Result<T> = ::std::result::Result<T, Error>;
#[derive(Debug)]
pub enum Error {
    Io(::std::io::Error),
}
impl From<::std::io::Error> for Error {
    fn from(v: ::std::io::Error) -> Self { Error::Io(v) }
}
impl ::std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
        Error::Io(e) => write!(f, "IO Error: {}", e),
        }
    }
}

/// Parse a file into a parsed module file
pub fn parse_file(input: &::std::path::Path) -> Result<ParsedModuleFile>
{
    let mut lex = lex::Lexer::from_path(input)?;
    let mut top_attrs = Vec::new();
    let rv = ParsedModuleFile {
        module: items::parse_module(&mut lex, &mut top_attrs)?,
        self_attrs: top_attrs,
        };
    if lex.peek().is_some() {
        todo!("Error: Stray characters")
    }
    Ok(rv)
}
