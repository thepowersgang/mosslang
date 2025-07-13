
#[derive(Debug,Clone)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
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

#[derive(Debug,Clone)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub enum Root
{
    None,   // Relative
    Current,
    Super(usize),
    Root,
}

#[derive(Clone,Hash)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub struct AbsolutePath(pub Vec<crate::Ident>);
impl AbsolutePath {
    pub fn append(&self, v: crate::Ident) -> AbsolutePath {
        AbsolutePath(self.0.iter().cloned().chain(::std::iter::once(v)).collect())
    }
    pub fn parent(&self) -> AbsolutePath {
        assert!(self.0.len() > 1);  // NOTE: Empty AbsolutePath isn't really valid, so only allow with two more more entries
        AbsolutePath(self.0[..self.0.len()-1].to_owned())
    }
}
impl ::core::fmt::Debug for AbsolutePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ::core::fmt::Display::fmt(self, f)
    }
}
impl ::core::fmt::Display for AbsolutePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.0 {
            write!(f, "::{}", c)?;
        }
        Ok(())
    }
}
#[derive(Clone,Debug)]
pub enum ValueBinding
{
    Local(u32),
    Function(AbsolutePath),
    Static(AbsolutePath),
    Constant(AbsolutePath),
    ///// Constructor for a tuple-like struct
    //StructValue(AbsolutePath),
    /// Enum value - convertible to integer
    ValueEnumVariant(AbsolutePath, usize),
    /// Enum value - function pointer
    DataEnumVariant(AbsolutePath, usize),
}
#[derive(Clone, Debug)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub enum TypeBinding
{
    Alias(AbsolutePath),
    Union(AbsolutePath),
    Struct(AbsolutePath),
    /// An enum that can be converted to an integer
    ValueEnum(AbsolutePath),
    /// An enum that carries data
    DataEnum(AbsolutePath),
    /// A data enum variant - only for pattern matching
    EnumVariant(AbsolutePath, usize)
}
