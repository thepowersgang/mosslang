//! Abstract Syntax Tree
//! 
//! A near 1:1 conversion of code into a tree

pub mod expr;
pub mod items;
pub mod ty;
pub mod path;

pub use self::expr::ExprRoot;
pub use self::ty::Type;
pub use self::path::Path;

pub struct Crate {
    pub attributes: Vec<Attribute>,
    pub module: items::Module,

    // TODO: Parsed crate attributes
}

pub type AbiSpec = Option<Vec<u8>>;

#[derive(Debug)]
pub struct Pattern {
    pub bindings: Vec<crate::Ident>,
    pub ty: PatternTy,
}
#[derive(Debug)]
pub enum PatternTy {
    Any,
    MaybeBind(crate::Ident),
    NamedValue(Path),
    Tuple(Vec<Pattern>),
}

pub struct Attribute
{
    pub name: crate::Ident,
    pub data: AttributeData,
}
pub enum AttributeData
{
    None,
    Value(Vec<u8>),
}