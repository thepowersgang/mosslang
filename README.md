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
- Library formats
  - Lazy option: Parse a "header" into a module
- Should formatting be improved?
  - Probably not: Don't want feature creep.
- Should autoderef be available?
  - Currently using `.*.` in place of C's `->`
  - Massive footgun to autoderef through multiple pointer layers.
  - buuut... this language will be a walking footgun.
- Slices?
  - Nice in theory, hard in practice
  - Option 1: Avoid C's mixing pointers and arrays by using slice syntax, but with no passed length?
  - Option 2: Have a variant of slice syntax that has metadata, and another that does not.
- Macros (rust-style? C-style?)
  - C style is nasty text manipulation
  - Rust style is a large can of worms