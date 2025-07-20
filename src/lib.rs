//! This library allows you to define custom deriving instances using
//! `macro_rules!` macros rather than proc-macros. This is often much simpler.
//! 
//! # Getting started
//! 
//! Define a deriving macro with `macro_rules!()`:
//! 
//! ```ignore
//! macro_rules! MyTrait {
//!   (/* see `rules_derive` for definition of signature */) => {
//!     // Generate impl
//!     impl $($generics_bindings)* MyTrait for $ty where $($generics_where)* {
//!       // implementation here
//!     }
//!   }
//! }
//! ```
//! 
//! Then use it under the `rules_derive` attribute:
//! 
//! ```ignore
//! #[rules_derive(MyTrait)]
//! struct MyType { x: u32, y: String }
//! ```
//! 
//! The macro definition can be in the same crate or file as its use.
//! 
//! See full examples [in the `examples` directory](https://github.com/MatX-inc/rules_derive/tree/main/examples).
//! 
//! # Tutorial
//! 
//! See the [announcement blog post](http://matx.com/research/rules_derive) for a tutorial.
//! 
//! # Parsed syntax
//! 
//! The `rules_derive` macro parses any `enum`/`struct` definition into a simpler-to-parse format, which it then
//! passes to your macro. The primary transformations it does are:
//! 
//! * Convert all enum/struct syntaxes and named-field/unnamed-field/unit syntaxes into a uniform sum-of-products
//!   syntax.
//! * Convert any generic parameters in the typical ways needed for `impl` headers.
//! 
//! The motivation for these transformations is given in the [announcement blog post](http://matx.com/research/rules_derive).
//! Here is an example of the effect of this transformation:
//! 
//! ```ignore
//! // Rust type definition:
//! #[rustfmt::skip]
//! pub enum Foo<T: Clone = u8> where u8: Into<T> { 
//!     A { x: T },
//!     B,
//!     C(u8),
//! }
//! 
//! // rules_derive-transformed type definition:
//! ((#[rustfmt::skip])) 
//! pub enum Foo((Foo<T>) (<T: Clone>) where (u8: Into<T>,))
//! {
//!     A(named Foo::A) { field__x @ x : T, } 
//!     B(unit Foo::B) {}
//!     C(unnamed Foo::C) { field__0 @ 0 : u8, }
//! }
//! ```
//! 
//! This transformed type definition is then passed to your macro. You can see the `macro_rules!` header
//! that accepts this transformed type definition on the [`rules_derive`] documentation.

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
/// Each derivable trait must be implemented by a macro named e.g. `Foo`, which
/// should accept the following protocol. You typically want to copy-paste this
/// header. You may also accept specalizations of the protocol, e.g. removing
/// some of the repetitions (to specialize for a specific number of fields or
/// variants) or specializing `$tystyle` to `struct` or `enum`.
///
/// TODO: remove redundant parens around (a) attr:tt, (b) `ty, generics_bindings where generics_where`.
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
/// Here is an example of a Rust type definition being transformed into this protocol:
/// 
/// Here is an example of the effect of this transformation:
/// 
/// ```ignore
/// // Rust type definition:
/// #[rustfmt::skip]
/// pub enum Foo<T: Clone = u8> where u8: Into<T> { 
///     A { x: T },
///     B,
///     C(u8),
/// }
/// 
/// // rules_derive-transformed type definition:
/// ((#[rustfmt::skip])) 
/// pub enum Foo((Foo<T>) (<T: Clone>) where (u8: Into<T>,))
/// {
///     A(named Foo::A) { field__x @ x : T, } 
///     B(unit Foo::B) {}
///     C(unnamed Foo::C) { field__0 @ 0 : u8, }
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
/// See full examples [in the `examples` directory](https://github.com/MatX-inc/rules_derive/tree/main/examples).
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
/// 
/// For example, `make_ident!(foo, "bar", 123)` becomes `foobar123`.
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
/// the source location attached to `foo`. The `foo` must be a single token-tree.
///
/// The most common use case is to attribute missing instances in a
/// `rules_derive` macro to a particular field of the source type. For this use
/// case, you should use the `$fieldty` token-tree as the source span to
/// attach to.
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
