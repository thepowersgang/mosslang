#[derive(Clone)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub struct Type
{
    pub kind: TypeKind,
}
impl Type
{
    pub fn new_void() -> Self {
        Type {
            kind: TypeKind::Void,
        }
    }
    pub fn new_unit() -> Self {
        Type {
            kind: TypeKind::Tuple(vec![]),
        }
    }
    pub fn new_infer() -> Self {
        Type {
            kind: TypeKind::Infer { index: None, kind: InferKind::None },
        }
    }
    pub fn new_path(p: super::Path) -> Self {
        Type {
            kind: TypeKind::Named(p, None),
        }
    }
    
    pub fn new_tuple(inner: Vec<Type>) -> Self {
        Type {
            kind: TypeKind::Tuple(inner),
        }
    }
    
    pub fn new_bool() -> Self {
        Type {
            kind: TypeKind::Bool,
        }
    }
    pub fn new_integer(cls: IntClass) -> Self {
        Type {
            kind: TypeKind::Integer(cls),
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

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, is_debug: bool) -> std::fmt::Result {
        match &self.kind {
        TypeKind::Infer { kind, index } => {
            f.write_str("_")?;
            if let Some(index) = index {
                write!(f, "#{}", index)?
            }
            else {
                write!(f, "#?")?
            }
            match kind {
            InferKind::None => Ok(()),
            InferKind::Integer => f.write_str("i"),
            InferKind::Float => f.write_str("f"),
            }
        },
        TypeKind::Bool => f.write_str("bool"),
        TypeKind::Void => f.write_str("void"),
        TypeKind::NullPointer => f.write_str("nullptr_t"),
        TypeKind::Integer(int_class) => match int_class
            {
            IntClass::PtrInt => f.write_str("usize"),
            IntClass::PtrDiff => f.write_str("isize"),
            IntClass::Signed(shift) => write!(f, "i{}", 8 << *shift),
            IntClass::Unsigned(shift) => write!(f, "u{}", 8 << *shift),
            },
        TypeKind::Tuple(items) => {
            f.write_str("( ")?;
            for t in items {
                t.fmt(f, is_debug)?;
                f.write_str(", ")?;
            }
            f.write_str(")")
        },
        TypeKind::Named(path, type_binding) => {
            if let Some(tb) = type_binding {
                use super::path::TypeBinding;
                let (t, path) = match tb {
                    TypeBinding::Alias(absolute_path) => ("alias", absolute_path),
                    TypeBinding::Union(absolute_path) => ("union", absolute_path),
                    TypeBinding::Struct(absolute_path) => ("struct", absolute_path),
                    TypeBinding::DataEnum(absolute_path) => ("enum[d]", absolute_path),
                    TypeBinding::ValueEnum(absolute_path) => ("enum[v]", absolute_path),
                    TypeBinding::EnumVariant(absolute_path, _) => ("variant", absolute_path),
                    };
                if is_debug {
                    write!(f, "{}/*{}*/", path, t)
                }
                else {
                    write!(f, "{}", path)
                }
            }
            else {
                write!(f, "{:?}", path)
            }
        },
        TypeKind::Pointer { is_const, inner } => {
            f.write_str(if *is_const { "*const " } else { "*mut "})?;
            inner.fmt(f, is_debug)
        },
        TypeKind::Array { inner, count } => {
            f.write_str("[")?;
            inner.fmt(f, is_debug)?;
            f.write_str(";")?;
            match count {
            ArraySize::Unevaluated(_) => todo!(),
            ArraySize::Known(v) => write!(f, "{}", v)?,
            }
            f.write_str("]")
        },
        }
    }
}

impl ::core::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt(f, true)
    }
}
impl ::core::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt(f, false)
    }
}

#[derive(Copy,Clone,Debug)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
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
pub enum ArraySize {
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
impl Eq for ArraySize {
}
impl PartialEq for ArraySize {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == ::std::cmp::Ordering::Equal
    }
}
impl PartialOrd for ArraySize {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for ArraySize {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (self, other) {
        (Self::Unevaluated(_l0), Self::Unevaluated(_r0)) => todo!("Compare ArraySize::Unevaluated"),
        (Self::Unevaluated(_), _) => ::std::cmp::Ordering::Less,
        (Self::Known(l0), Self::Known(r0)) => l0.cmp(r0),
        (Self::Known(_), _) => ::std::cmp::Ordering::Greater,
        }
    }
}

#[derive(Clone,Debug)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub enum TypeKind
{
    /// An omitted type
    Infer {
        kind: InferKind,
        index: Option<usize>,
    },
    /// A type that cannot exist, used for untyped pointers
    Void,
    /// Boolean - Return type of comparison operations, and input for `if`
    Bool,
    /// Integers
    Integer(IntClass),
    //Float(FloatClass),
    /// A tuple - anonymous collection of values
    Tuple(Vec<Type>),
    /// A named type
    Named(super::Path, Option<super::path::TypeBinding>),
    /// Type returned by the integer literal `0p`
    /// 
    /// Can coerce to any pointer type
    NullPointer,
    /// Pointer to data
    Pointer {
        is_const: bool,
        inner: Box<Type>,
    },
    /// Array (homogenous collection of data)
    Array {
        inner: Box<Type>,
        count: ArraySize,
    }
}
#[derive(Clone,Debug)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub enum InferKind {
    /// No type-class information available
    None,
    /// This ivar must be an integer type
    Integer,
    /// This ivar must be a floating point type
    Float,
}