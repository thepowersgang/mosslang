#[derive(Debug)]
pub struct Pattern {
    pub span: crate::Span,
    pub bindings: Vec<PatternBinding>,
    pub ty: PatternTy,
    /// Used by type inference to store a type
    pub data_ty: super::Type,
}
pub struct PatternBinding {
    pub name: crate::Ident,
    pub index: Option<u32>,
}
impl ::std::fmt::Debug for PatternBinding {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(i) = self.index {
            write!(f, "{}#{}", self.name, i)
        }
        else {
            write!(f, "{}#?", self.name)
        }
    }
}
#[derive(Debug)]
pub enum PatternTy {
    Any,
    Multiple(Vec<Pattern>),
    MaybeBind(crate::Ident),
    Tuple(Vec<Pattern>),
    ValueSingle(Value),
    ValueRangeExcl(Value, Value),
    ValueRangeIncl(Value, Value),
}

#[derive(Debug)]
pub enum Value {
    Integer(u128),
    NamedValue(NamedValue),
}

#[derive(Debug)]
pub enum NamedValue {
    Unbound(super::Path),
    Constant(super::path::AbsolutePath),
    EnumVariant(super::path::AbsolutePath, usize),
}