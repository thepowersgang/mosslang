
use ::std::rc::Rc;

pub type TypeInfoRef = Rc<TypeInfo>;

pub struct TypeInfo {
    size: usize,
    align: usize,
    /// If `fields` is empty, then this is a primitive (pointer, float, or integer)
    /// If there is only one field, and it's size doesn't match the outer size - this is an array
    fields: Structure,
    // TODO: Support tagged unions with variant information
    variants: Option<Variants>,
}
enum Structure {
    Primitive,
    Array(Rc<TypeInfo>),
    Composite(Vec<(usize,Rc<TypeInfo>,)>),
}
struct Variants {
    tag_field: usize,
    max_variant: usize,
}
impl TypeInfo {
    pub fn make_primitive(int_class: crate::ast::ty::IntClass) -> Self {
        use crate::ast::ty::IntClass;
        let bytes_log2 = match int_class {
            IntClass::PtrInt|IntClass::PtrDiff => 3,
            IntClass::Signed(s) => s,
            IntClass::Unsigned(s) =>s,
            };
        let bytes = 1 << bytes_log2;
        TypeInfo {
            size: bytes,
            align: bytes,
            fields: Structure::Primitive,
            variants: None,
        }
    }
    pub fn make_composite(size: usize, align: usize, fields: Vec<(usize,Rc<TypeInfo>,)>) -> Self {
        TypeInfo {
            size,
            align,
            fields: Structure::Composite(fields),
            variants: None,
        }
    }
    pub fn make_array(count: usize, inner: TypeInfoRef) -> Self {
        TypeInfo {
            size: count * inner.size,
            align: inner.align,
            fields: Structure::Array(inner),
            variants: None
        }
    }
    
    pub fn size(&self) -> usize {
        self.size
    }
    pub fn align(&self) -> usize {
        self.align
    }
}