
pub fn load_crate(path: &::std::path::Path) -> ::std::io::Result<crate::ast::Crate> {
    let b = ::std::fs::read(path)?;
    let module = ::bson::deserialize_from_slice(&b).map_err(|e| ::std::io::Error::other(e))?;
    Ok(crate::ast::Crate {
        attributes: Vec::new(),
        module,
        externals: Default::default(),
    })
}
pub fn save_crate(path: &::std::path::Path, ast_crate: &crate::ast::Crate) -> ::std::io::Result<()> {
    let b = ::bson::serialize_to_vec(&ast_crate.module).map_err(|e| ::std::io::Error::other(e))?;
    ::std::fs::write(path, b)
}

impl<'de> ::serde::Deserialize<'de> for crate::Span {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>
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
        D: serde::Deserializer<'de>
    {
        struct V;
        impl<'a> ::serde::de::Visitor<'a> for V {
            type Value = crate::Ident;

            fn visit_borrowed_str<E: serde::de::Error>(self, v: &'a str) -> Result<Self::Value, E> {
                Ok(crate::Ident::from_str(v))
            }
        
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                todo!()
            }
        }
        deserializer.deserialize_str(V)
    }
}
impl ::serde::Serialize for crate::Ident {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer
    {
        serializer.serialize_str(&format!("{}", self))
    }
}


// --- ConstantValue ---
impl<'de> ::serde::Deserialize<'de> for crate::ast::items::ConstantValue {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error>
    {
        struct V;
        impl<'a> ::serde::de::Visitor<'a> for V {
            type Value = crate::ast::items::EvaluatedConstant;

            fn visit_byte_buf<E: serde::de::Error,>(self, v: Vec<u8>) -> Result<Self::Value, E>
            {
                Ok(crate::ast::items::EvaluatedConstant(v))
            }

            fn visit_borrowed_bytes<E: serde::de::Error,>(self, v: &[u8]) -> Result<Self::Value, E>
            {
                self.visit_byte_buf(v.to_owned())
            }
        
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a byte array for ConstantValue/EvaluatedConstant")
            }
        }
        Ok(crate::ast::items::ConstantValue::Evaluated(deserializer.deserialize_byte_buf(V)?))
    }
}
impl ::serde::Serialize for crate::ast::items::ConstantValue {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        match self {
        crate::ast::items::ConstantValue::Unknown(_) => panic!("Unexpanded value?"),
        crate::ast::items::ConstantValue::Evaluated(evaluated_constant) => {
            serializer.serialize_bytes(&evaluated_constant.0)
        },
        }
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

// --- TypePath ---
impl<'de> ::serde::Deserialize<'de> for crate::ast::ty::TypePath {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(crate::ast::ty::TypePath::Resolved(crate::ast::path::TypeBinding::deserialize(deserializer)?))
    }
}
impl ::serde::Serialize for crate::ast::ty::TypePath {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        let crate::ast::ty::TypePath::Resolved(p) = self else { panic!("Unresolved type path: {:?}", self); };
        p.serialize(serializer)
    }
}

// --- ExprInType ---
impl<'de> ::serde::Deserialize<'de> for crate::ast::ty::ExprInType {
    fn deserialize<D: serde::Deserializer<'de>>(_deserializer: D) -> Result<Self, D::Error> {
        panic!("typeof shouldn't be serialised")
    }
}
impl ::serde::Serialize for crate::ast::ty::ExprInType {
    fn serialize<S: serde::Serializer>(&self, _serializer: S) -> Result<S::Ok, S::Error> {
        panic!("typeof shouldn't be serialised")
    }
}