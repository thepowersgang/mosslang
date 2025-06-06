
Project name:
- "rust--" - Trademark issues - it's not rust
- "r--" - R is another language, nope.
- "rs--" - Nah...
- "moss" - It's an sticky fungus :D

A modern C, but with rust-like syntax
- Very close to full rust syntax 
  - Add post-fix deref syntax to make derefs explicit, but still ergonomic
  - For loops need thought, for now they're just magic syntax
- Aim for C ABI compatability

## Goals
- A simple language spec (close to C)
- Fewer parsing issues than C has (mainly by moving types to after names)
- C ABI compatability


## Features
- Rust-ish syntax
- Type inference, with no coercions

## Non-Features
- Trait generics, too much effort to implement
  - Duck-typed generics are possible, although they make parts of inference harder. C++ has to monomorphise to do type resolution.
  - Although, no generics would fall into the trap Go found.
  - But this isn't mean for the same things as Go
- Trait objects - if you want vtables, do it yourself
- Operator overrides - messy without traits to control them.
- Borrow checker - obviously


# Questions
- Generics?
  - C++ duck-typed generics could work, although they can be footguns.
  - A better syntax will take some of the issues away.
  - Turbofish `:(`
- Namespaces?
- Enums (and pattern matching)?
  - Would allow `Option`/`Result` error handling.
  - And if duck generics are allowed, it would help.
  - But, without a borrow checker would enums cause footguns?
  - Without borrows/non-null pointers, cannot compact.
  - Could require all pointers be non-null and use Option to handle null?
- Slices?
  - Slices in rust are unsized fat pointers, which has ABI questions.
  - But, slices avoid C's biggest issue - arrays
- Formatting?
  - Could use printf... but that's not type safe. And requires C strings.
  - Could use a builtin, but that's HACKY.
  - Locally defined version of printf that takes string slices, but then what level of type checking?
- C Strings?
  - Option: define all string literals as ending in NUL?
  - Option: Only byte strings (utf is allowed, but not enforced)
- For loops?
  - Could use rust's range syntax as part of a special for loop syntax
    - `for var in start .. end` translating to `for(auto var = start; var != end; var ++)`
    - What about other step sizes?
- NULL pointers?
  - Could just have `0` coerce to nullptr

- IDEA: Have an unsized array type and don't treat pointers as arrays like C does
  - Pointer arithmatic is still "allowed", but indexing requires an array type?

## Option: Just make this C with rust syntax
- No slices, no generics.
  - Generics open a lot of questions
    - Turbofish? It's been a contentious point
    - Traits add complexity to everything
    - C++ duck-typing requires monomorph before type resolution.
  - Slices are hard without generics
- Data-bearing enums are possible.


