
pub fn load_crate(path: &::std::path::Path) -> ::std::io::Result<crate::ast::Crate> {
    todo!()
}
pub fn save_crate(path: &::std::path::Path, krate: &crate::ast::Crate) -> ::std::io::Result<()> {
    todo!()
}

impl<'de> ::serde::Deserialize<'de> for crate::Span {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: serde::Deserializer<'de>
    {
        Ok(Self::new_extern())
    }
}
impl ::serde::Serialize for crate::Span {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer
    {
        serializer.serialize_unit()
    }
}

impl<'de> ::serde::Deserialize<'de> for crate::Ident {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        todo!()
    }
}
impl ::serde::Serialize for crate::Ident {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        todo!()
    }
}



impl<'de> ::serde::Deserialize<'de> for crate::ast::items::ConstantValue {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        todo!()
    }
}
impl ::serde::Serialize for crate::ast::items::ConstantValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        todo!()
    }
}



impl<'de> ::serde::Deserialize<'de> for crate::ast::ty::ArraySize {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        todo!()
    }
}
impl ::serde::Serialize for crate::ast::ty::ArraySize {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        todo!()
    }
}

impl<'de> ::serde::Deserialize<'de> for crate::ast::ty::TypePath {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        todo!()
    }
}
impl ::serde::Serialize for crate::ast::ty::TypePath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        todo!()
    }
}
impl<'de> ::serde::Deserialize<'de> for crate::ast::ty::ExprInType {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de> {
        panic!("typeof shouldn't be serialised")
    }
}
impl ::serde::Serialize for crate::ast::ty::ExprInType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        panic!("typeof shouldn't be serialised")
    }
}