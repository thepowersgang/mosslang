pub struct Type
{

}
impl Type
{
    pub fn new_unit() -> Self {
        Type {}
    }
    pub fn new_infer() -> Self {
        Type {}
    }
    pub fn new_path(p: super::Path) -> Self {
        Type {}
    }

    pub fn new_ptr(is_const: bool, inner: Type) -> Self {
        Type {}
    }
    pub fn new_array(inner: Type, count: crate::ast::ExprRoot) -> Self {
        Type {}
    }
    pub fn new_slice(inner: Type) -> Self {
        Type {}
    }
}