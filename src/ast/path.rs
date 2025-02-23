
#[derive(Debug)]
pub struct Path
{
    pub root: Root,
    pub components: Vec<crate::Ident>,
}

impl Path
{
    pub fn is_trivial(&self) -> bool {
        matches!(self.root, Root::None) && self.components.len() == 1
    }
    pub fn into_trivial(self) -> Result<crate::Ident, Self> {
        if self.is_trivial() {
            Ok(self.components.into_iter().next().unwrap())
        }
        else {
            Err(self)
        }
    }
}

#[derive(Debug)]
pub enum Root
{
    None,   // Relative
    Current,
    Super(usize),
    Root,
}

#[derive(Clone)]
pub struct AbsolutePath(pub Vec<crate::Ident>);
impl AbsolutePath {
    pub fn append(&self, v: crate::Ident) -> AbsolutePath {
        AbsolutePath(self.0.iter().cloned().chain(::std::iter::once(v)).collect())
    }
}
#[derive(Clone)]
pub enum ValueBinding
{
    Local(usize),
    Function(AbsolutePath),
    Static(AbsolutePath),
    Constant(AbsolutePath),
    StructValue(AbsolutePath),
    EnumVariant(AbsolutePath, usize)
}
#[derive(Clone)]
pub enum TypeBinding
{
    Alias(AbsolutePath),
    Union(AbsolutePath),
    Struct(AbsolutePath),
    Enum(AbsolutePath),
    EnumVariant(AbsolutePath, usize)
}
