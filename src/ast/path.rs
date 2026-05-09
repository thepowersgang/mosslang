
#[derive(Debug,Clone)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
#[derive(serde::Deserialize,serde::Serialize)]
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
#[derive(serde::Deserialize,serde::Serialize)]
pub enum Root
{
    /// No prefix to the path, same as `current`?
    None,   // Relative
    /// Relative to the current module (i.e. `self::`)
    Current,
    /// Relative to a parent module, the count is the number of parents to skip (i.e. `Super(0)` means `super::`)
    Super(usize),
    /// Relative to the crate root (`crate::` or `::foo`)
    Root,
}

#[derive(Clone,Hash)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
#[derive(serde::Deserialize,serde::Serialize)]
pub struct AbsolutePath(Option<crate::Ident>, pub Vec<crate::Ident>);
impl AbsolutePath {
    pub fn new_current() -> Self {
        AbsolutePath(None, Default::default())
    }
    pub fn new_extern(name: crate::Ident) -> Self {
        AbsolutePath(Some(name),Default::default())
    }
    pub fn append(&self, v: crate::Ident) -> AbsolutePath {
        AbsolutePath(self.0.clone(), self.1.iter().cloned().chain(::std::iter::once(v)).collect())
    }
    pub fn parent(&self) -> AbsolutePath {
        assert!(self.1.len() > 0);
        AbsolutePath(self.0.clone(), self.1[..self.1.len()-1].to_owned())
    }
    pub fn parent_n(&self, extra_count: usize) -> Self {
        assert!(self.1.len() > extra_count);
        AbsolutePath(self.0.clone(), self.1[..self.1.len()-1-extra_count].to_owned())
    }
}
impl ::core::fmt::Debug for AbsolutePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        ::core::fmt::Display::fmt(self, f)
    }
}
impl ::core::fmt::Display for AbsolutePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(n) = &self.0 {
            write!(f, "::{}", n)?;
        }
        else {
            f.write_str("crate")?;
        }
        for c in &self.1 {
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
#[derive(serde::Serialize,serde::Deserialize)]
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
