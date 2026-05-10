//! AST Items - Top level constructs (functions, statics, ...)
use crate::Ident;

#[derive(Default)]
#[derive(serde::Deserialize,serde::Serialize)]
pub struct Module
{
    pub items: Vec<Item>,
}

/// A module-level AST item
#[derive(serde::Deserialize,serde::Serialize)]
pub struct Item
{
    #[serde(skip)]
    pub attributes: Vec<super::Attribute>,
    pub name: Option<crate::Ident>,
    pub ty: ItemType,
}
/// Item type for a module-level item
#[derive(serde::Deserialize,serde::Serialize)]
pub enum ItemType
{
    Module(Module),
    Use(Use),

    ExternCrate(Ident), // TODO: Have another reference counted string?
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

#[derive(serde::Deserialize,serde::Serialize)]
pub struct Use
{
    pub items: Vec<UseItem>,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct UseItem {
    pub span: crate::Span,
    pub path: super::Path,
    pub ty: UseTy,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub enum UseTy {
    AsIs,
    Renamed(crate::Ident),
    Glob,
}

#[derive(serde::Deserialize,serde::Serialize)]
pub struct ExternBlock
{
    pub abi: super::AbiSpec,
    pub items: Vec<ExternItem>,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct ExternItem
{
    #[serde(skip)]
    pub attributes: Vec<super::Attribute>,
    pub name: crate::Ident,
    pub ty: ExternItemType,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub enum ExternItemType
{
    Function(FunctionSignature),
    Static(ExternStatic),
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct ExternStatic {
    pub ty: super::Type,
}

#[derive(serde::Deserialize,serde::Serialize)]
pub struct Struct
{
    /// Struct fields
    pub fields: Vec<StructField>,
    /// Indicates that the struct is incomplete (aka semi-opaque)
    pub is_incomplete: bool,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct StructField
{
    #[serde(skip)]
    /// Attributes on the field
    pub attributes: Vec<super::Attribute>,
    /// Field name
    pub name: Ident,
    /// Field data type
    pub ty: super::Type,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct Union
{
    /// Union variants
    pub variants: Vec<StructField>,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct Enum
{
    pub variants: Vec<EnumVariant>,
    /// Indicates that the enum doesn't represent all possible values (and that users must consider non-mentioned values)
    pub is_incomplete: bool,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct EnumVariant
{
    #[serde(skip)]
    pub attributes: Vec<super::Attribute>,
    pub name: Ident,
    pub ty: EnumVariantTy,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub enum EnumVariantTy
{
    Bare,
    Value(ConstantValue),
    Data(super::Type),
}

#[derive(serde::Deserialize,serde::Serialize)]
pub struct Static
{
    pub ty: super::Type,
    #[serde(skip, default="ConstantValue::external")]
    pub value: ConstantValue,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct Constant
{
    pub ty: super::Type,
    pub value: ConstantValue,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct Function
{
    pub sig: FunctionSignature,

    #[serde(skip, default="crate::ast::ExprRoot::new_extern")]
    pub code: crate::ast::ExprRoot,
}
#[derive(serde::Deserialize,serde::Serialize)]
pub struct FunctionSignature
{
    /// ABI selected (note: if this is in an ExternBlock, this will be None)
    pub abi: super::AbiSpec,
    pub args: Vec<(super::Pattern, super::Type)>,
    pub is_variadic: bool,
    pub ret: super::Type,
}


pub enum ConstantValue {
    Unknown(crate::ast::ExprRoot),
    Evaluated(EvaluatedConstant),
}
impl ConstantValue {
    fn external() -> Self {
        Self::Evaluated(EvaluatedConstant(vec![]))
    }
}
#[derive(Debug)]
pub struct EvaluatedConstant(pub Vec<u8>);