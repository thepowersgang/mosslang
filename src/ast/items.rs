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
    Module(Module),

    ExternBlock(ExternBlock),

    TypeAlias(),
    Struct(Struct),
    Enum(Enum),
    Union(Union),
    Function(Function),
    Static(Static),
    // Note: No impl blocks or traits
}

pub struct ExternBlock
{
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
    pub fields: Vec<StructField>,
}
pub struct StructField
{
    pub attributes: Vec<super::Attribute>,
    pub name: Ident,
    //type: Type,
}
pub struct Union
{
}
pub struct Enum
{
    pub variants: Vec<EnumVariant>,
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
    Value(super::expr::ExpressionTy),
    Named(Struct),
}

pub struct Static
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
    //pub abi: Option<Vec<u8>>,
    pub args: Vec<(super::Pattern, super::Type)>,
    pub is_variadic: bool,
    pub ret: super::Type,
}
