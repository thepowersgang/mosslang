
#[derive(Debug)]
pub struct Path
{
    pub root: Root,
    pub components: Vec<crate::Ident>,
}

impl Path
{
    pub fn is_trivial(&self) -> bool {
        matches!(self.root, Root::None) && self.components.is_empty()
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
