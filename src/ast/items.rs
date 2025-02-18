//! AST Items - Top level constructs (funtions, statics, ...)
use crate::Ident;

#[derive(Default)]
pub struct Module
{
    pub items: Vec<Item>,
}

/// A module-level AST item
pub struct Item
{
    pub attributes: Vec<super::Attribute>,
    pub name: Option<crate::Ident>,
    pub ty: ItemType,
}
/// Item type for a module-level item
pub enum ItemType
{
    //Module(Module),

    ExternBlock(ExternBlock),

    TypeAlias(crate::ast::Type),
    Struct(Struct),
    Enum(Enum),
    Union(Union),
    Function(Function),
    Static(Static),
    Constant(Constant),
    // Note: No impl blocks or traits
}

pub struct ExternBlock
{
    pub abi: super::AbiSpec,
    pub items: Vec<ExternItem>,
}
pub struct ExternItem
{
    pub attributes: Vec<super::Attribute>,
    pub name: crate::Ident,
    pub ty: ExternItemType,
}
pub enum ExternItemType
{
    Function(FunctionSignature),
    Static(ExternStatic),
}
pub struct ExternStatic {
    pub ty: super::Type,
}

pub struct Struct
{
    /// Struct fields
    pub fields: Vec<StructField>,
    /// Indicates that the struct is incomplete (aka semi-opaque)
    pub is_incomplete: bool,
}
pub struct StructField
{
    /// Attributes on the field
    pub attributes: Vec<super::Attribute>,
    /// Field name
    pub name: Ident,
    /// Field data type
    pub ty: super::Type,
}
pub struct Union
{
}
pub struct Enum
{
    pub variants: Vec<EnumVariant>,
    pub is_incomplete: bool,
}
pub struct EnumVariant
{
    pub attributes: Vec<super::Attribute>,
    pub name: Ident,
    pub ty: EnumVariantTy,
}
pub enum EnumVariantTy
{
    Bare,
    Value(super::expr::ExprRoot),
    Named(Struct),
}

pub struct Static
{
    pub ty: super::Type,
    pub value: crate::ast::ExprRoot,
}
pub struct Constant
{
    pub ty: super::Type,
    pub value: crate::ast::ExprRoot,
}
pub struct Function
{
    pub sig: FunctionSignature,

    pub code: crate::ast::ExprRoot,
}
pub struct FunctionSignature
{
    /// ABI selected (note: if this is in an ExternBlock, this will be None)
    pub abi: super::AbiSpec,
    pub args: Vec<(super::Pattern, super::Type)>,
    pub is_variadic: bool,
    pub ret: super::Type,
}
