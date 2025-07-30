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

# Comparison with Alternatives

## Overview

The table below compares `rules_derive` with other approaches for creating derive macros in Rust:

| Feature | [syn](https://docs.rs/syn/latest/syn/)/[quote](https://docs.rs/quote/latest/quote/) | rules_derive | [macro_rules_attribute](https://docs.rs/macro_rules_attribute/latest/macro_rules_attribute/)/[RFC 3698](https://github.com/rust-lang/rfcs/blob/192c533633bb81aa3010b30e49f828d6cde36807/text/3698-declarative-derive-macros.md) | [derive-deftly](https://docs.rs/derive-deftly/1.2.0/derive_deftly/) | [synstructure](https://docs.rs/synstructure/latest/synstructure/) | [crabtime](https://docs.rs/crabtime/latest/crabtime/) |
|---------|-----------|--------------|--------------------------------|---------------|--------------|----------|
| Quality: **Generic type (`Foo<T>`) support** | ✅ | ✅ | ❌ | ✅ | ✅ | ❌ |
| Quality: **Good error attribution** | ✅ | ✅ | ❌ | ❌ | ✅ | ❌ |
| Quality: **Attribute parsing** | ✅ | 🚧 | ❌ | ✅ | ✅ | ❌ |
| **Dependency size** | ~40k LoC | <1k LoC | <1k LoC | 60k+ LoC | 60k+ LoC | Huge (rustc) |
| Ergonomics: **Same-crate derivers** | ❌ | ✅ | ✅ | ✅ | ❌ | ✅ |
| Ergonomics: **Uniform type syntax** | ❌ | ✅ | ❌ | ✅ | ✅ | ❌ |
| Generality: **Opinionated approach** | No | No | No | Yes (custom template language) | Yes (fold/foreach abstractions) | No |

**Legend:** ✅ Yes, ❌ No, 🚧 Under construction

We aim for `rules_derive` to be a viable candidate for implementing derivers for *core ecosystem libraries* such as [zerocopy](https://docs.rs/zerocopy/latest/zerocopy/), [strum](https://docs.rs/strum/latest/strum/), [serde](https://docs.rs/serde/latest/serde/). All such libraries today use the standard syn/quote approach rather than any of the alternatives, and we believe this is because syn/quote is the *smallest high-quality approach*: smallest means the smallest transitive dependency size; and high-quality means that derivers have generic type support, good error attribution, and (perhaps) attribute parsing. As we see it, rules_derive is the first approach that meets the syn/quote quality bar while also being a far *smaller* dependency.

The ecosystem advantage that might be possible by a smaller high-quality approach is that core ecosystem libraries might get to a point where they no longer need to carefully feature-gate all their use of derive macros.

## Detailed Comparisons

### syn/quote

`syn`/`quote` is by far the standard approach. rules_derive is far smaller of a dependency and is typically also more concise, but we aim to hit the same quality bar as syn/quote derivers for the vast majority of use cases. For some long tail use cases, e.g. ones that run complex algorithms at macro time, syn/quote will likely remain more flexible long-term.

### synstructure

`synstructure` builds on syn/quote but imposes a fold/foreach-based programming model that from our perspective is a little *too* opinionated. We share with `synstructure` the idea of expressing all types as a sum-of-products (or list of variants).

### derive-deftly

`derive-deftly` shares many core ideas with rules_derive: macro_rules-based implementation plus a single-time proc-macro that parses type definitions into a uniform syntax. The biggest difference is that `derive-deftly` is based on a new template language for derivers that is similar to `macro_rules` but but slightly different and with richer templating features such as loops and ifs. In contrast, `rules_derive` stays with Rust's `macro_rules` language, and when we want to add new functionality we add new Rust macros. Ultimately this becomes a tradeoff between features and simplicity: `rules_derive` is 800 lines of code and has zero dependencies, whereas `derive-deftly` is 8k lines of code and has transitive dependencies of ~60k lines of code.

### macro_rules_attribute, RFC 3698

`macro_rules_attribute` is similarly lightweight to Rust, but does not provide any support for parsing generic types, transforming type definitions to a uniform syntax, or offering good error attribution. This seems to make it a reasonable fit for simple examples, but not for producing deriving macros of similar quality to what you can do with syn/quote.

`RFC 3698` would bake the approach of `macro_rules_attribute` into the Rust language, but again does not tackle the "awkward squad" of generic types, uniform type definition syntax, error attribution.

### crabtime

`crabtime` offers a very nice custom syntax for defining macros---substantially nicer than the `macro_rules!` syntax that Rust ships with---but its implementation is heavyweight: it is a proc macro that internally invokes the Rust compiler as a subprocess. This is far beyond the normal hermeticity expectations of a proc macro, and seems like it may pose challenges for other build environments such as `bazel` which enforce strong isolation of build actions.