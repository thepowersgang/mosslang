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

#[derive(Debug)]
pub struct Pattern {
    pub bindings: Vec<crate::Ident>,
    pub ty: PatternTy,
}
#[derive(Debug)]
pub enum PatternTy {
    MaybeBind(crate::Ident),
    NamedValue(Path),
}

pub struct Attribute
{
}