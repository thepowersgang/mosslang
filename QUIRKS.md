Random notes of non-trivial differences to rust

Syntax
- Postifx dereference using `.*` i.e. a field named `*`
- Enums and structs can end with `...` preventing exhaustive matches (and indicating that the enum is an open option set)
- `0p` is equivalent to C++'s `nullptr` value

Semantics
- Integer/float types can coerce to larger types