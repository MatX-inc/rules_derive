//! This library allows you to define custom deriving instances using
//! `macro_rules!` macros rather than proc-macros. This is often much simpler.
//!
//! To define a custom deriving instance using this library:
//!
//! ```ignore
//! // Define a macro that accepts a simplified form of the type definition as its argument.
//! macro_rules! Foo {
//!   /* ... see `rules_derive` for definition of signature ... */
//! }
//!
//! // Use the macro on your types:
//! #[rules_derive(Foo, Bar, Baz)]
//! struct MyType { ... }
//! ```
//!
//! For a complete example, see `tests/custom_clone_and_eq.rs`, which
//! provides deriving instances for custom equivalents of `Clone` and `Eq`. As a
//! nice bonus, the derived instances have _better_ type signatures than Rust's
//! default instances in the presence of phantom types (`PhantomData`).
//!
//! # Features
//!
//! This library provides the following functionality, which you would typically
//! need to implement yourself when writing a traditional `#[derive(Foo)]`
//! proc-macro:
//!
//! 1. You don't need to write a proc macro. Writing proc macros adds some
//!    boilerplate, since you need to set up a separate crate, or often two
//!    separate crates since proc macros cannot be written in the same crate as
//!    non-macro exports.
//!
//! 2. Traditionally you'd need to write code to handle each of the six kinds of
//!    type definition:
//!     * unit structs: `struct Foo;`
//!     * tuple structs: `struct Foo(u32, u32);`
//!     * named-field structs: `struct Foo { x: u32, y: u32 }`
//!     * unit enum variants: `enum Foo { A, B, C }`
//!     * tuple enum variants: `enum Foo { A(u32), B(), C(u32, u32) }
//!     * named-field enum variants: `enum Foo { A { x: u32, y: u32 }, B, C { x:
//!       u32, y: u32 } }`
//!
//!     This library takes care of this for you, by canonicalizing all of these
//!     different syntaxes into a single sum-of-products representation.
//!
//! 3. Traditionally you'd need to parse the type's generic arguments and
//!    generate a few variants for different uses. For example, given a type
//!    definition  ```ignore struct GenericStruct<T: Copy = u8, const N: usize>
//!    { ... } ```
//!
//!    you'd need to generate an `impl` header like the following:
//!
//!    ```ignore
//!    impl<T: Copy, const N: usize> Foo for GenericStruct<T, N> { ... }
//!    ```
//!
//!    This library takes care of this for you, generating a syntax tree for
//!    the fully-applied type `GenericStruct<T, N>` as well as the impl
//!    bindings `<T: Copy, const N: usize>`.
//!
//! Relative to writing a `derive_Foo! { struct MyType { ... } }` macro, this
//! library provides the following advantages:
//!
//! 4. Generics are supported. Generic type signatures are extremely difficult
//!    to parse directly in `macro_rules!` macros, and so most `derive_Foo!()`
//!    macros don't support them. With this library, they're straightforward to
//!    support.
//!
//! 5. Multiple traits can be derived for a single type. Typically `derive_Foo!
//!    { derive_Bar! { struct MyType { ...} } }` is not supported, but with this
//!    library you can write
//!
//!     ```ignore
//!     #[rules_derive(Foo, Bar)]
//!     struct MyType { ... }
//!     ```
//!    without any issues.
//!
//! # Comparison
//!
//! The most similar library to this is `synstructure`, which is aimed at
//! features (2) and (3) but not (1). As an additional issue with
//! `synstructure`, I found that `synstructure` automates _too much_ of the
//! definition of an impl's header, such that it often ends up generating a bad
//! signature in the presence of generics and phantom types.

use proc_macro::Delimiter;
use proc_macro::Group;
use proc_macro::Ident;
use proc_macro::TokenStream;
use proc_macro::TokenTree;
mod parsing;
use parsing::*;

/// Provides `#[rules_derive(Foo, Bar)]` to derive `Foo` and `Bar` for a struct
/// or enum.
///
/// Each derivable trait must be implemented by a macro named `Foo`, which
/// should accept the following protocol. You typically want to copy-paste this
/// header. You may also accept specalizations of the protocol, e.g. removing
/// some of the repetitions (to specialize for a specific number of fields or
/// variants) or specializing `$tystyle` to `struct` or `enum`.
///
/// TODO: remove redundant parens around (a) attr:tt, (b) `ty, generics_bindings
/// where generics_where``.
///
/// The full protocol:
///
/// ```no_run
/// macro_rules! Foo {
///   (
///     ($( ($($attr:tt)*) )*)
///     $vis:vis $tystyle:ident $name:ident (($ty:ty) ($($generics_bindings:tt)*) where ($($generics_where:tt)*)) {
///       $(
///         $variant_name:ident ($variant_style:ident $($qualified_variant:tt)*) $(= ($discriminant:expr))? {
///           $(
///             $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
///           )*
///         }
///       )*
///     }
///   ) => { ... }
/// }
/// ```
///
/// If parameters are provided to the rules_derive macro name in parentheses,
/// square brackets, or curly braces (e.g. `rules_derive(Iterator(Item = (I, &'a
/// T)))`), the contents of the parameter list are passed verbatim to the macro
/// before the type description, enclosed in curly braces, so a macro definition
/// that matches calls with the above parameters looks like:
///
/// ```no_run
/// macro_rules! Iterator {
///   (
///     { Item = $item_ty:ty }
///     ... /* type description protocol */ ...
///   ) => { ... }
/// }
/// ```
///
/// See `tests/custom_clone_and_eq.rs` and `examples/heap_size.rs` for examples;
/// see `Iterator` in `common/src/derives.rs` for an example of using
/// additional parameters.
#[proc_macro_attribute]
pub fn rules_derive(attr: TokenStream, item: TokenStream) -> TokenStream {
  let mut result = vec![
    punct('#'),
    brackets([
      ident("derive"),
      parens([
        punct_joint(':'),
        punct(':'),
        ident("rules_derive"),
        punct_joint(':'),
        punct(':'),
        ident("__RulesDerive"),
      ]),
    ]),
    punct('#'),
    brackets([ident("__rules_derive"), parens(attr)]),
  ];
  result.extend(item);
  result.into_iter().collect()
}

/// Internal proc macro that implements the `#[rules_derive(...)]` attribute.
///
/// Even though the `#[rules_derive(...)]` user-facing syntax
/// (i.e. it's a proc_macro_attribute) is ideal, we want the semantics of
/// proc_macro_derive, because Rust resolves #[cfg(...)] attributes after
/// proc_macro_attribute but before proc_macro_derive. Hence we bounce
/// #[rules_derive(...)] through `#[derive(::rules_derive::__RulesDerive)]
/// #[__rules_derive(...)]` to get the #[cfg(...)] attributes to resolve
/// correctly.
#[doc(hidden)]
#[proc_macro_derive(__RulesDerive, attributes(__rules_derive))]
pub fn rules_derive_impl(item: TokenStream) -> TokenStream {
  render_macro_result(rules_derive_inner(item))
}

fn rules_derive_inner(item: TokenStream) -> Result<TokenStream> {
  // Following https://doc.rust-lang.org/reference/items.html, we parse `Item`, which must
  // be `Struct`, or `Enumeration`.
  //
  // Item →
  //   OuterAttribute* ( VisItem | MacroItem )
  // VisItem →
  //   Visibility?
  //   (
  //     ...
  //     | Struct
  //     | Enumeration
  //   )
  let mut item = ParseState::new(item.into_iter());
  let mut description = Vec::new();
  let attr: TokenStream;

  // OuterAttribute*
  {
    let mut attrs = Vec::new();
    attr = parse_item_attributes(&mut item, &mut attrs)?;
    description.push(parens(attrs));
  }

  // Visibility?
  parse_visibility(&mut item, &mut description);

  // Struct → StructStruct | TupleStruct
  //
  // StructStruct → struct IDENTIFIER GenericParams? WhereClause? ( {
  //   StructFields? } | ; )
  //
  // TupleStruct →
  //   struct IDENTIFIER GenericParams? `(` TupleFields? `)` WhereClause? ;
  //
  // Enumeration → enum IDENTIFIER GenericParams? WhereClause? { EnumItems? }
  //
  // Parse `struct` or `enum`.
  let TokenTree::Ident(tystyle_ident) = item.next()? else {
    return item.error(&"expected `struct` or `enum` for #[rules_derive]");
  };
  enum TyStyle {
    Struct,
    Enum,
  }
  let tystyle = match tystyle_ident.to_string().as_str() {
    "struct" => TyStyle::Struct,
    "enum" => TyStyle::Enum,
    "union" => {
      return Err(Msg(
        tystyle_ident.span(),
        &"`union` is not supported by #[rules_derive]",
      ))
    }
    _ => {
      return Err(Msg(
        tystyle_ident.span(),
        &"expected `struct` or `enum` for #[rules_derive]",
      ))
    }
  };
  description.push(TokenTree::Ident(tystyle_ident));

  // Parse type name.
  let type_name = item.next()?;
  description.push(type_name.clone());

  // GenericParams?
  let (ty, generics_bindings) = parse_generics(&mut item, &type_name)?;

  // StructStruct → ... WhereClause? ( {
  //   StructFields? } | ; )
  //
  // TupleStruct → ... `(` TupleFields? `)` WhereClause? ;
  //
  // Enumeration → ... WhereClause? { EnumItems? }
  //
  // We parse `WhereClause?` now. In the TupleStruct we may have to come back and
  // parse it again.
  let mut generics_where = Vec::new();
  parse_where_clause(&mut item, &mut generics_where)?;

  let mut variants = Vec::new();

  match item.next()? {
    TokenTree::Group(group) => {
      match group.delimiter() {
        proc_macro::Delimiter::Brace => match tystyle {
          TyStyle::Struct => {
            // StructStruct → ... { StructFields? }
            let fields = parse_named_fields(group.stream())?;
            variants.push(type_name.clone());
            variants.push(parens([with_span(group.span_open(), ident("named")), type_name.clone()]));
            variants.push(braces(fields));
          }
          TyStyle::Enum => {
            // Enumeration → ... { EnumItems? }
            // EnumItems → EnumItem ( , EnumItem )* ,?
            let mut input_variants = ParseState::new(group.stream().into_iter());
            while !input_variants.at_end() {
              // EnumItem →
              //   OuterAttribute* Visibility?
              //   IDENTIFIER ( EnumItemTuple | EnumItemStruct )? EnumItemDiscriminant?
              skip_outer_attributes(&mut input_variants)?; // TODO: attributes

              // `Visibility?` is actually illegal here.
              let variant_name = input_variants.next()?; // IDENTIFIER
              let (variant_metadata, fields) =
                if let Some(TokenTree::Group(_)) = &input_variants.peeks[0] {
                  let TokenTree::Group(enum_item) = input_variants.must_next() else {
                    unreachable!()
                  };
                  match enum_item.delimiter() {
                    proc_macro::Delimiter::Parenthesis => {
                      // Input: EnumItemTuple → `(` TupleFields? `)`
                      // Output:
                      // variant_metadata: ($variant_style:ident $($qualified_variant:tt)*)
                      // fields: {   $(
                      //     $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
                      //   )*
                      // }
                      let variant_metadata = parens([
                        with_span(enum_item.span(), ident("unnamed")),
                        type_name.clone(),
                        punct_joint(':'),
                        punct(':'),
                        variant_name.clone(),
                      ]);
                      let fields = braces(parse_unnamed_fields(enum_item.stream())?);
                      (variant_metadata, fields)
                    }
                    proc_macro::Delimiter::Brace => {
                      // Input: EnumItemStruct → { StructFields? }
                      // Output:
                      // variant_metadata: ($variant_style:ident $($qualified_variant:tt)*)
                      // fields: {   $(
                      //     $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
                      //   )*
                      // }
                      let variant_metadata = parens([
                        with_span(enum_item.span(), ident("named")),
                        type_name.clone(),
                        punct_joint(':'),
                        punct(':'),
                        variant_name.clone(),
                      ]);
                      let fields = braces(parse_named_fields(enum_item.stream())?);
                      (variant_metadata, fields)
                    }
                    _ => {
                      return input_variants.error(&"Parse error");
                    }
                  }
                } else {
                  // Unit variant.
                  let variant_metadata = parens([
                    with_span(input_variants.span(), ident("unit")),
                    type_name.clone(),
                    punct_joint(':'),
                    punct(':'),
                    variant_name.clone(),
                  ]);
                  let fields = braces([]);
                  (variant_metadata, fields)
                };

              let mut discriminant = Vec::new();
              if input_variants.peek_punct(0, b"=") {
                input_variants.copy(1, &mut discriminant)?; // '='
                let mut inner_discriminant = Vec::new();
                parse_expression(&mut input_variants, &mut inner_discriminant)?;
                discriminant.push(parens(inner_discriminant));
              }
              // $variant_name:ident ($variant_style:ident $($qualified_variant:tt)*) $(=
              // ($discriminant:expr))? {   $(
              //     $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
              //   )*
              // }
              variants.push(variant_name);
              variants.push(variant_metadata);
              variants.extend(discriminant);
              variants.push(fields);

              if input_variants.peek_punct(0, b",") {
                input_variants.next().unwrap(); // ','
              }
            }
          }
        },
        proc_macro::Delimiter::Parenthesis => {
          // TupleStruct → ... `(` TupleFields? `)` WhereClause? ;
          //
          // Output:
          // $variant_name:ident ($variant_style:ident $($qualified_variant:tt)*) $(=
          // ($discriminant:expr))? {   $(
          //     $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
          //   )*
          // }
          variants.push(type_name.clone()); // $variant_name
          variants.push(parens([with_span(group.span_open(), ident("unnamed")), type_name.clone()]));
          variants.push(braces(parse_unnamed_fields(group.stream())?));
          parse_where_clause(&mut item, &mut generics_where)?;
          item.next()?; // ';'
        }
        _ => {
          return item.error(&"Parse error");
        }
      }
    }
    TokenTree::Punct(punct) => {
      if punct.as_char() != ';' {
        return item.error(&"Parse error");
      }
      // Unit struct.
      variants.push(type_name.clone()); // $variant_name
      variants.push(parens([with_span(punct.span(), ident("unit")), type_name.clone()]));
      variants.push(braces([]));
    }
    _ => return item.error(&"Parse error"),
  }
  description.push(parens([
    parens(ty),
    parens(generics_bindings),
    ident("where"),
    parens(generics_where),
  ]));
  description.push(braces(variants));

  // Parse `#[rules_derive(Foo(...), Bar)]` attribute.
  let mut derives = Vec::new();
  let mut attr = ParseState::new(attr.into_iter());
  while !attr.at_end() {
    // macro_name ( ... ),
    let mut extra_args = None;
    let mut is_empty = true;
    while let Ok(t) = attr.next() {
      derives.push(t);
      is_empty = false;
      if let Some(TokenTree::Group(_)) = &attr.peeks[0] {
        let Ok(TokenTree::Group(group)) = attr.next() else {
          unreachable!()
        };
        extra_args = Some(group.stream());
        break;
      }
      if attr.at_end() || attr.peek_punct(0, b",") {
        break;
      }
    }
    if attr.peek_punct(0, b",") {
      attr.next()?; // ','
    }
    if is_empty {
      break;
    }
    derives.push(punct('!'));
    let mut description_with_args = Vec::new();
    if let Some(extra_args) = extra_args {
      description_with_args.push(braces(extra_args));
    }
    description_with_args.extend(description.clone());
    derives.push(parens(description_with_args));
    derives.push(punct(';'));
  }

  Ok(derives.into_iter().collect())
}

/// Creates an identifier from the concatenation of identifiers and literals.
#[proc_macro]
pub fn make_ident(item: TokenStream) -> TokenStream { render_macro_result(make_ident_inner(item)) }

fn make_ident_inner(item: TokenStream) -> Result<TokenStream> {
  let mut name = String::new();
  for t in item {
    match t {
      TokenTree::Ident(ident) => {
        name.push_str(&ident.to_string());
      }
      TokenTree::Literal(literal) => {
        name.push_str(&literal.to_string());
      }
      _ => {
        return Err(Msg(
          t.span(),
          &"make_ident! only accepts idents and literals",
        ));
      }
    }
  }
  Ok(TokenTree::Ident(Ident::new(&name, proc_macro::Span::call_site())).into())
}

/// Provides a way for `macro_rules` to annotate pieces of syntax with source
/// spans, so that you may improve error messages to point to the correct source
/// span.
///
/// Within the scope of an outer `with_spans!(...)` invocation, you may use
/// `spanned!(foo => tokens...)` to cause the `tokens...` to be annotated with
/// the source location attached to `foo`. See `tests/custom_clone_and_eq.rs`
/// and `examples/heap_size.rs`. The `foo` is required to be an identifier, to
/// avoid surprising span attributions that often arise from other syntax
/// structures.
///
/// The most common use case is to attribute missing instances in a
/// `rules_derive` macro to a particular field of the source type. For this use
/// case, you should use the `$fieldnameident` token as the source span to
/// attach to: the `$fieldnameident` token has been set up with a source
/// span that points to the entire field definition.
///
/// Why provide `with_spans!(...)` as a proc macro instead of directly providing
/// `spanned!(...)` as a proc macro? Because the latter is too restrictive: Rust
/// only allows a limited number of syntactic places where a macro invocation is
/// allowed, and we need to use `spanned!(...)` in locations where macro
/// invocations are not allowed, such as impl "where" clauses. The two-level
/// `with_spans!(... spanned!(...) ...)` approach allows us to work around this
/// limitation.
#[proc_macro]
pub fn with_spans(item: TokenStream) -> TokenStream {
  let mut result = Vec::new();
  let result = process_stream(None, ParseState::new(item.into_iter()), &mut result)
    .map(|()| result.into_iter().collect());
  render_macro_result(result)
}

// A traversal of the token tree that tracks the `span` of the closest ancestor
// `spanned!(foo => ...)` and recursively applies that to all tokens in the
// tree.
//
// We simultaneously search for the `spanned!(...)` pattern of tokens (3
// consecutive tokens) and apply span adjustments to all tokens in the tree.
//
// We append to `dst` rather than returning it, so as to avoid redundant copying
// (which would be a two-pass algorithm) when we flatten a `spanned!(...)`
// subtree into its parent.
fn process_stream(
  span: Option<proc_macro::Span>,
  mut stream: ParseState,
  dst: &mut Vec<TokenTree>,
) -> Result<()> {
  'token_loop: while !stream.at_end() {
    if stream.peek_ident(0, "spanned") && stream.peek_punct(1, b"!") && stream.peek_group(2) {
      // We got a match for spanned!(...).
      stream.must_next(); // spanned
      stream.must_next(); // !
      if let TokenTree::Group(group) = stream.must_next() {
        let mut inner_stream = ParseState::new(group.stream().into_iter());
        if let Ok(mut spanned_tt) = inner_stream.next() {
          // Look inside arbitrarily many outer nests of {} or invisible groups.
          //
          // macro_rules!() matching introduces invisible groups on metavariables such as
          // ty/expr, which groups together multiple tokens. We also allow users
          // to group together multiple spanned tokens with {} delimiters. In both
          // cases we want to see inside these layers of delimiters to get the spans of
          // the underlying tokens.
          let inner_span;
          loop {
            if let TokenTree::Group(g) = &spanned_tt {
              if matches!(g.delimiter(), Delimiter::None | Delimiter::Brace) {
                let mut delimiter_contents = g.stream().into_iter();
                let first = delimiter_contents.next();
                let last = delimiter_contents.fold(None, |_prev, next| Some(next));
                match (first, last) {
                  (Some(first), Option::None) => {
                    // There's just a single token. Recurse inside to check if that itself is a
                    // delimiter that we might look through.
                    spanned_tt = first;
                    continue;
                  }
                  (Some(first), Some(last)) => {
                    // Multiple tokens inside. Use them to set the span and then stop recursing.
                    #[cfg(feature = "nightly")]
                    {
                      inner_span = first.span().join(last.span());
                    }
                    #[cfg(not(feature = "nightly"))]
                    {
                      inner_span = first.span();
                      _ = last;
                    }
                    break;
                  }
                  _ => {
                    // No tokens inside. Stop recursing.
                  }
                }
              }
            }
            inner_span = spanned_tt.span();
            break;
          }
          if inner_stream.peek_punct(0, b"=>") {
            inner_stream.must_next(); // =
            inner_stream.must_next(); // >
            process_stream(Some(inner_span), inner_stream, dst)?;
            continue 'token_loop;
          }
        }
      }
      return stream.error(&"spanned!() must match syntax spanned!($span:tt => ...)");
    }
    dst.push(process_tree(span, stream.must_next())?);
  }
  Ok(())
}

fn process_tree(span: Option<proc_macro::Span>, tree: TokenTree) -> Result<TokenTree> {
  let mut tree = match tree {
    TokenTree::Group(g) => {
      let mut inner_dst = Vec::new();
      process_stream(
        span,
        ParseState::new(g.stream().into_iter()),
        &mut inner_dst,
      )?;
      TokenTree::Group(Group::new(g.delimiter(), inner_dst.into_iter().collect()))
    }
    tree => tree,
  };
  if let Some(span) = span {
    tree.set_span(tree.span().located_at(span));
  }
  Ok(tree)
}
