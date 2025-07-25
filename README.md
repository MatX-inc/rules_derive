# rules_derive: simple and fast derive macros using `macro_rules`

This library allows you to define custom deriving instances using
`macro_rules!` macros rather than proc-macros. This is often much simpler.

# Getting started

Define a deriving macro with `macro_rules!()`:

```rust
macro_rules! MyTrait {
  (/* see `rules_derive` for definition of signature */) => {
    // Generate impl
    impl $($generics_bindings)* MyTrait for $ty where $($generics_where)* {
      // implementation here
    }
  }
}
```

Then use it under the `rules_derive` attribute:

```rust
#[rules_derive(MyTrait)]
struct MyType { x: u32, y: String }
```

The macro definition can be in the same crate or file as its use.

See full examples [in the `examples` directory](/examples).

# Tutorial

See the [announcement blog post](http://matx.com/research/rules_derive) for a tutorial.

# Parsed syntax

The `rules_derive` macro parses any `enum`/`struct` definition into a simpler-to-parse format, which it then
passes to your macro. The primary transformations it does are:

* Convert all enum/struct syntaxes and named-field/unnamed-field/unit syntaxes into a uniform sum-of-products
  syntax.
* Convert any generic parameters in the typical ways needed for `impl` headers.

The motivation for these transformations is given in the [announcement blog post](http://matx.com/research/rules_derive).
Here is an example of the effect of this transformation:

```rust
// Rust type definition:
#[rustfmt::skip]
pub enum Foo<T: Clone = u8> where u8: Into<T> { 
    A { x: T },
    B,
    C(u8),
}

// rules_derive-transformed type definition:
((#[rustfmt::skip])) 
pub enum Foo((Foo<T>) (<T: Clone>) where (u8: Into<T>,))
{
    A(named Foo::A) { field__x @ x : T, } 
    B(unit Foo::B) {}
    C(unnamed Foo::C) { field__0 @ 0 : u8, }
}
```

This transformed type definition is then passed to your macro. You can see the `macro_rules!` header
that accepts this transformed type definition on the [`rules_derive`] documentation.
