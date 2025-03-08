#[derive(Clone, Debug)]
pub struct Type
{
    kind: TypeKind,
}
impl Type
{
    pub fn new_unit() -> Self {
        Type {
            kind: TypeKind::Tuple(vec![]),
        }
    }
    pub fn new_infer() -> Self {
        Type {
            kind: TypeKind::Infer { explicit: false },
        }
    }
    pub fn new_path(p: super::Path) -> Self {
        Type {
            kind: TypeKind::Named(p),
        }
    }
    
    pub fn new_tuple(inner: Vec<Type>) -> Self {
        Type {
            kind: TypeKind::Tuple(inner),
        }
    }

    pub fn new_ptr(is_const: bool, inner: Type) -> Self {
        Type {
            kind: TypeKind::Pointer { is_const, inner: Box::new(inner) }
        }
    }
    pub fn new_array(inner: Type, count: crate::ast::ExprRoot) -> Self {
        Type {
            kind: TypeKind::Array { inner: Box::new(inner), count: ArraySize::Unevaluated(Box::new(count)), }
        }
    }
    //pub fn new_slice(inner: Type) -> Self {
    //    Type {}
    //}
}

#[derive(Clone,Debug)]
pub enum IntClass {
    /// rust's usize
    PtrInt,
    /// rust's isize
    PtrDiff,
    /// Signed integer with explicit size (size stored as log2 bytes)
    Signed(u8),
    /// Unsigned integer with explicit size (size stored as log2 bytes)
    Unsigned(u8),
}

#[derive(Debug)]
enum ArraySize {
    Unevaluated(Box<crate::ast::ExprRoot>),
    Known(usize),
}
impl Clone for ArraySize {
    fn clone(&self) -> Self {
        match *self {
        Self::Unevaluated(_) => todo!("Clone unevaluated array size"),
        Self::Known(s) => Self::Known(s),
        }
    }
}

#[derive(Clone,Debug)]
enum TypeKind
{
    Infer {
        explicit: bool,
    },
    Integer(IntClass),
    Tuple(Vec<Type>),
    Named(super::Path),
    Pointer {
        is_const: bool,
        inner: Box<Type>,
    },
    Array {
        inner: Box<Type>,
        count: ArraySize,
    }
}