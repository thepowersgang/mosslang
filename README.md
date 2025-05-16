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


# Comparison to C
- Type inference
- Modern syntax
  - Sensible pointer syntax
  - Trailing commas allowed
  - Braces enforced, no parens required
- No concept of "undefined behavior", although some behaviours can be unspecified
  - This means that the compiler cannot assume that UB cannot happen
  - but, what C would call UB will still do unexpected things (e.g. write to random memory)

# Open questions
- Should formatting be improved?
- Should autoderef be available?
  - Currently using `.*.` in place of C's `->`
- Slices?