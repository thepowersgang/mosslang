#[derive(Clone)]
pub struct Type
{
    pub span: crate::Span,
    pub kind: TypeKind,
}
impl PartialOrd for Type {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.kind.partial_cmp(&other.kind)
    }
}
impl Ord for Type {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.kind.cmp(&other.kind)
    }
}
impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
    }
}
impl Eq for Type {
}
impl Type
{
    pub fn new_void(span: crate::Span) -> Self {
        Type {
            kind: TypeKind::Void,
            span,
        }
    }
    pub fn new_unit(span: crate::Span) -> Self {
        Type {
            kind: TypeKind::Tuple(vec![]),
            span,
        }
    }
    pub fn new_infer(span: crate::Span) -> Self {
        Type {
            kind: TypeKind::Infer { index: None },
            span,//: crate::Span::new_null(),
        }
    }
    pub fn new_path(span: crate::Span, p: super::Path) -> Self {
        Type {
            kind: TypeKind::Named(TypePath::Unresolved(p)),
            span,
        }
    }
    pub fn new_path_resolved(span: crate::Span, p: super::path::TypeBinding) -> Self {
        Type {
            kind: TypeKind::Named(TypePath::Resolved(p)),
            span,
        }
    }
    
    pub fn new_tuple(span: crate::Span, inner: Vec<Type>) -> Self {
        Type {
            kind: TypeKind::Tuple(inner),
            span,
        }
    }
    
    pub fn new_bool(span: crate::Span) -> Self {
        Type {
            kind: TypeKind::Bool,
            span,
        }
    }
    pub fn new_integer(span: crate::Span, cls: IntClass) -> Self {
        Type {
            kind: TypeKind::Integer(cls),
            span,
        }
    }

    pub fn new_typeof(span: crate::Span, expr: super::ExprRoot) -> Self {
        Type {
            kind: TypeKind::TypeOf(ExprInType(Box::new(expr))),
            span,
        }
    }
    pub fn new_ptr(span: crate::Span, is_const: bool, inner: Type) -> Self {
        Type {
            kind: TypeKind::Pointer { is_const, inner: Box::new(inner) },
            span,
        }
    }

    pub fn new_array_expr(span: crate::Span, inner: Type, count: crate::ast::ExprRoot) -> Self {
        Type {
            kind: TypeKind::Array { inner: Box::new(inner), count: ArraySize::Unevaluated(Box::new(count)), },
            span,
        }
    }
    pub fn new_array_fixed(span: crate::Span, inner: Type, count: usize) -> Self {
        Type {
            kind: TypeKind::Array { inner: Box::new(inner), count: ArraySize::Known(count), },
            span,
        }
    }
    pub fn new_array_unsized(span: crate::Span, inner: Type) -> Self {
        Type {
            kind: TypeKind::UnsizedArray( Box::new(inner) ),
            span,
        }
    }
    //pub fn new_slice(span: crate::Span, inner: Type) -> Self {
    //    Type {}
    //}

    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, is_debug: bool) -> std::fmt::Result {
        match &self.kind {
        TypeKind::Infer { index } => {
            f.write_str("_")?;
            if let Some(index) = index {
                write!(f, "#{}", index)
            }
            else {
                write!(f, "#?")
            }
        },
        TypeKind::Bool => f.write_str("bool"),
        TypeKind::Void => f.write_str("void"),
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
        TypeKind::Named(path) => {
            match path {
            TypePath::Resolved(tb) => {
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
                },
            TypePath::Unresolved(path) => {
                write!(f, "{:?}", path)
                },
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
        TypeKind::UnsizedArray(inner) => {
            f.write_str("[")?;
            inner.fmt(f, is_debug)?;
            f.write_str("; ...")?;
            f.write_str("]")
        },
        TypeKind::TypeOf(inner) => {
            write!(f, "typeof({:?})", inner.0)
        }
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
impl IntClass {
    pub fn is_signed(&self) -> bool {
        match self {
        IntClass::PtrInt => false,
        IntClass::PtrDiff => true,
        IntClass::Signed(_) => true,
        IntClass::Unsigned(_) => false,
        }
    }
}

#[derive(Debug)]
pub enum ArraySize {
    Unevaluated(Box<crate::ast::ExprRoot>),
    Known(usize),
}
impl Clone for ArraySize {
    fn clone(&self) -> Self {
        match *self {
        Self::Unevaluated(ref a) => todo!("{}: Clone unevaluated array size", a.e.span),
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

#[derive(Debug)]
pub struct ExprInType(pub Box<super::ExprRoot>);
impl Clone for ExprInType {
    fn clone(&self) -> Self {
        todo!("Clone expression in type")
    }
}
impl Eq for ExprInType {
}
impl PartialEq for ExprInType {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == ::std::cmp::Ordering::Equal
    }
}
impl PartialOrd for ExprInType {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for ExprInType {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        todo!("Compare `ExprInType`")
    }
}

#[derive(Clone,Debug)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub enum TypeKind
{
    /// An omitted type
    Infer {
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
    Named(TypePath),
    /// Pointer to data
    Pointer {
        is_const: bool,
        inner: Box<Type>,
    },
    /// Array (homogenous collection of data)
    Array {
        inner: Box<Type>,
        count: ArraySize,
    },
    /// An array with no size specified (similar to rust's slice type, but doesn't imply metadata)
    UnsizedArray(Box<Type>),
    /// Evaluates to the type of an expression (during typecheck)
    TypeOf(ExprInType),
}
#[derive(Clone,Debug)]
#[derive(PartialOrd,Ord,PartialEq,Eq)]
pub enum TypePath {
    Unresolved(super::Path),
    Resolved(super::path::TypeBinding),
}