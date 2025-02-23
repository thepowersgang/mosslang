A design experiment on a "minimal rust", using rust's general syntax - but closer to C's semantics.

# Features
- Rust syntax (prefixed item types, suffixed )
- Scoped enums
- Type inference
- Strict (-ish) type checking
  - Probably will support coercions to convert to/from void pointers
- Tuples
- Pattern matching

# Non-features
- Impl blocks (no methods)
- Generics (would need monomorph and traits)
- Traits, because no generics
- Borrows (and borrowchecker)