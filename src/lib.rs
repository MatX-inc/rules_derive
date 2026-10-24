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
//! The `rules_derive` macro parses any `enum`/`struct` definition into a
//! simpler-to-parse format, which it then passes to your macro. The primary
//! transformations it does are:
//!
//! * Convert all enum/struct syntaxes and named-field/unnamed-field/unit
//!   syntaxes into a uniform sum-of-products syntax.
//! * Convert any generic parameters in the typical ways needed for `impl`
//!   headers.
//!
//! The motivation for these transformations is given in the [announcement blog post](http://matx.com/research/rules_derive).
//! Here is an example of the effect of this transformation:
//!
//! ```ignore
//! // Rust type definition:
//! #[rustfmt::skip]
//! pub enum Foo<T: Clone = u8> where u8: Into<T> {
//!     A { #[t(json::rename(y))] x: T },
//!     /// The B variant.
//!     #[t(json::skip)]
//!     B,
//!     C(u8),
//! }
//!
//! // rules_derive-transformed type definition:
//! (() (#[rustfmt::skip]))
//! pub enum Foo ((Foo<T>) (<T: Clone>) (T: Clone) where (u8: Into<T>,))
//! {
//!     (()) A (named Foo::A) { ((json::rename(y),)) f_x @ x : T, }
//!     ((json::skip(),) (#[doc = " The B variant."])) B (unit Foo::B) {}
//!     (()) C (unnamed Foo::C) { (()) tuple_field_0 @ 0 : u8, }
//! }
//! ```
//!
//! The container, every variant and every field carry an attribute group
//! `( ($($t_entries)*) $( (#[$($attr)*]) )* )`. Its first element collects the
//! entries of all `#[t(...)]` attributes, each normalized to `path(args),`; the
//! remaining elements are the other attributes, each in its own parentheses. A
//! struct's single variant has the empty group `(())`. Read `#[t(...)]` entries
//! with [`attr_lookup!`], and declare the ones your deriver reads with
//! `declare_used_attributes!` (see [`rules_derive`]).
//!
//!//! The generics appear three times: `(Foo<T>)` for naming the type, `(<T:
//! Clone>)` (`generics_bindings`, with the angle brackets) for `impl` headers,
//! and `(T: Clone)` (`generics_inner`, without the angle brackets) for splicing
//! extra parameters into an `impl`.
//!
//! This transformed type definition is then passed to your macro. You can see
//! the `macro_rules!` header that accepts this transformed type definition on
//! the [`rules_derive`] documentation.

// `Span::join` (used by `spanned!` under the `nightly` feature to widen a span
// across multiple tokens) is gated behind the unstable `proc_macro_span`
// feature. Enable it only when the `nightly` feature is on.
#![cfg_attr(feature = "nightly", feature(proc_macro_span))]

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
/// The full protocol:
///
/// ```no_run
/// macro_rules! Foo {
///   (
///     $container_attributes:tt
///     $vis:vis $tystyle:ident $name:ident (($ty:ty) ($($generics_bindings:tt)*) ($($generics_inner:tt)*) where ($($generics_where:tt)*)) {
///       $(
///         $variant_attributes:tt
///         $variant_name:ident ($variant_style:ident $($qualified_variant:tt)*) $(= ($discriminant:expr))? {
///           $(
///             $field_attributes:tt
///             $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
///           )*
///         }
///       )*
///     }
///   ) => { ... }
/// }
/// ```
///
/// Here is an example of the effect of this transformation:
///
/// ```ignore
/// // Rust type definition:
/// #[rustfmt::skip]
/// pub enum Foo<T: Clone = u8> where u8: Into<T> {
///     A { #[t(json::rename(y))] x: T },
///     /// The B variant.
///     #[t(json::skip)]
///     B,
///     C(u8),
/// }
///
/// // rules_derive-transformed type definition:
/// (() (#[rustfmt::skip]))
/// pub enum Foo ((Foo<T>) (<T: Clone>) (T: Clone) where (u8: Into<T>,))
/// {
///     (()) A (named Foo::A) { ((json::rename(y),)) f_x @ x : T, }
///     ((json::skip(),) (#[doc = " The B variant."])) B (unit Foo::B) {}
///     (()) C (unnamed Foo::C) { (()) tuple_field_0 @ 0 : u8, }
/// }
/// ```
///
/// # Attributes
///
/// Each attribute group (`$container_attributes`, `$variant_attributes`,
/// `$field_attributes`) has the shape
///
/// ```text
/// ( ( $( $path(args), )* ) $( (#[$($ordinary_attribute)*]) )* )
/// ```
///
/// The first element collects the entries of every `#[t(...)]` attribute in
/// source order, each normalized to `path(args),` (`path(),` for a key written
/// without arguments, such as `#[t(json::skip)]`); one `#[t(...)]` may hold
/// several comma-separated entries. The other elements are the remaining
/// ("ordinary") attributes, including doc comments and any `#[repr(...)]` or
/// `#[derive(...)]` written after `#[rules_derive(...)]`, each wrapped in
/// parentheses. A struct's single variant has the empty group `(())`.
///
/// `#[t(...)]` is reserved for derivers: by convention each deriver uses its
/// own first path segment (`json::`, `sb::`, ...). Look keys up with
/// [`attr_lookup!`]; inside [`with_extra_macros!`] that works even in `where`
/// clauses.
///
/// A deriver declares which keys it reads by calling, unqualified,
///
/// ```ignore
/// declare_used_attributes!(field(json::rename, json::skip), variant(json::rename));
/// ```
///
/// in item position of its expansion; the categories are `container`,
/// `variant` and `field`. `#[rules_derive]` defines this macro ahead of the
/// deriver calls, together with a never-called function per `#[t(...)]` key
/// present on the type, spanned at the key. Declaring a key references its
/// function, so a key that no deriver declares (such as a misspelling) is
/// reported as an unused function pointing at the key.
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
#[proc_macro_derive(__RulesDerive, attributes(__rules_derive, t))]
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
  let mut t_attrs = Vec::new();

  // OuterAttribute*
  let mut rules_derive_args = None;
  description.push(parse_attributes(
    &mut item,
    AttrCategory::Container,
    &mut t_attrs,
    Some(&mut rules_derive_args),
  )?);
  let Some(attr) = rules_derive_args else {
    return item.error(&"Could not find internal #[__rules_derive(...)] attribute");
  };

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
  let (ty, generics_bindings, generics_inner) = parse_generics(&mut item, &type_name)?;

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
            let fields = parse_named_fields(group.stream(), &mut t_attrs)?;
            variants.push(parens([parens([])])); // $variant_attributes
            variants.push(type_name.clone());
            variants.push(parens([
              with_span(group.span_open(), ident("named")),
              type_name.clone(),
            ]));
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
              let variant_attributes = parse_attributes(
                &mut input_variants,
                AttrCategory::Variant,
                &mut t_attrs,
                None,
              )?;

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
                      let fields = braces(parse_unnamed_fields(enum_item.stream(), &mut t_attrs)?);
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
                      let fields = braces(parse_named_fields(enum_item.stream(), &mut t_attrs)?);
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
              variants.push(variant_attributes);
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
          variants.push(parens([parens([])])); // $variant_attributes
          variants.push(type_name.clone()); // $variant_name
          variants.push(parens([
            with_span(group.span_open(), ident("unnamed")),
            type_name.clone(),
          ]));
          variants.push(braces(parse_unnamed_fields(group.stream(), &mut t_attrs)?));
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
      variants.push(parens([parens([])])); // $variant_attributes
      variants.push(type_name.clone()); // $variant_name
      variants.push(parens([
        with_span(punct.span(), ident("unit")),
        type_name.clone(),
      ]));
      variants.push(braces([]));
    }
    _ => return item.error(&"Parse error"),
  }
  description.push(parens([
    parens(ty),
    parens(generics_bindings),
    parens(generics_inner),
    ident("where"),
    parens(generics_where),
  ]));
  description.push(braces(variants));

  // Parse `#[rules_derive(Foo(...), Bar)]` attribute.
  let mut derives = used_attributes_prelude(&type_name, &t_attrs);
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

/// Emits the definitions that support `declare_used_attributes!`: one never-
/// called function per distinct `#[t(...)]` key (named after the key and
/// spanned at it), and a `declare_used_attributes!` macro, in scope for the
/// derivers, which references the functions of the keys it declares. A key no
/// deriver declares is thus reported as an unused function, pointing at the
/// key.
fn used_attributes_prelude(type_name: &TokenTree, t_attrs: &[TAttr]) -> Vec<TokenTree> {
  let type_str = type_name.to_string();
  let type_str = type_str.strip_prefix("r#").unwrap_or(&type_str).to_string();
  let mut seen: Vec<(AttrCategory, &str)> = Vec::new();
  let mut result = Vec::new();
  let mut entries = Vec::new();
  for attr in t_attrs {
    if seen.contains(&(attr.category, attr.path.as_str())) {
      continue;
    }
    seen.push((attr.category, attr.path.as_str()));
    let fn_name = Ident::new(
      &format!(
        "unused_{}_attribute_{}_on_{}",
        attr.category.name(),
        attr.path.replace("::", "__"),
        type_str
      ),
      attr.span,
    );
    // #[allow(non_snake_case)] fn NAME() {}
    //
    // The whole item carries the key's span: rustc does not report dead code
    // in items whose span comes from a macro expansion.
    result.extend(
      [
        punct('#'),
        brackets([ident("allow"), parens([ident("non_snake_case")])]),
        ident("fn"),
        TokenTree::Ident(fn_name.clone()),
        parens([]),
        braces([]),
      ]
      .map(|t| with_span(attr.span, t)),
    );
    entries.push(parens([
      ident(attr.category.name()),
      string(&attr.path),
      TokenTree::Ident(fn_name),
    ]));
  }
  // #[allow(unused_macros)]
  // macro_rules! declare_used_attributes {
  //   ($($declarations:tt)*) => {
  //     ::rules_derive::__declare_used_attributes! { [ENTRIES] $($declarations)*
  // }   };
  // }
  let body = if entries.is_empty() {
    braces([])
  } else {
    braces([
      punct_joint(':'),
      punct(':'),
      ident("rules_derive"),
      punct_joint(':'),
      punct(':'),
      ident("__declare_used_attributes"),
      punct('!'),
      braces([
        brackets(entries),
        punct('$'),
        parens([punct('$'), ident("declarations")]),
        punct('*'),
      ]),
    ])
  };
  result.extend([
    punct('#'),
    brackets([ident("allow"), parens([ident("unused_macros")])]),
    ident("macro_rules"),
    punct('!'),
    ident("declare_used_attributes"),
    braces([
      parens([
        punct('$'),
        parens([punct('$'), ident("declarations"), punct(':'), ident("tt")]),
        punct('*'),
      ]),
      punct_joint('='),
      punct('>'),
      body,
      punct(';'),
    ]),
  ]);
  result
}

/// Internal: the expansion of `declare_used_attributes!` within a
/// `#[rules_derive]` expansion.
///
/// Input: `[ $( ($category:ident $path:literal $fn_name:ident) )* ]` (the keys
/// present on the type) followed by the deriver's declarations
/// `$( $category:ident ( $($path:path),* ) ),*`. Output: a reference to the
/// function of each present key that is declared, which marks it as used.
#[doc(hidden)]
#[proc_macro]
pub fn __declare_used_attributes(item: TokenStream) -> TokenStream {
  render_macro_result(declare_used_attributes_inner(item))
}

fn declare_used_attributes_inner(item: TokenStream) -> Result<TokenStream> {
  let mut input = ParseState::new(item.into_iter());
  let TokenTree::Group(present) = input.next()? else {
    return input.error(&"internal error: expected [...]");
  };
  let mut present_keys = Vec::new();
  for entry in present.stream() {
    let TokenTree::Group(entry) = entry else {
      continue;
    };
    let mut it = entry.stream().into_iter();
    let (Some(category), Some(path), Some(fn_name)) = (it.next(), it.next(), it.next()) else {
      continue;
    };
    let path = path.to_string();
    present_keys.push((
      category.to_string(),
      path.trim_matches('"').to_string(),
      fn_name,
    ));
  }

  let mut uses = Vec::new();
  while !input.at_end() {
    // $category ( $($path),* )
    let category = input.next()?;
    let category_str = category.to_string();
    if AttrCategory::from_name(&category_str).is_none() {
      return Err(Msg(
        category.span(),
        &"declare_used_attributes!: expected `container(...)`, `variant(...)` or `field(...)`",
      ));
    }
    let TokenTree::Group(paths) = input.next()? else {
      return Err(Msg(
        category.span(),
        &"declare_used_attributes!: expected `(path, ...)` after the category",
      ));
    };
    let mut paths = ParseState::new(paths.stream().into_iter());
    while !paths.at_end() {
      let mut path = String::new();
      while !paths.at_end() && !paths.peek_punct(0, b",") {
        let t = paths.must_next();
        let t = t.to_string();
        path.push_str(t.strip_prefix("r#").unwrap_or(&t));
      }
      if paths.peek_punct(0, b",") {
        paths.must_next();
      }
      for (present_category, present_path, fn_name) in &present_keys {
        if *present_category == category_str && *present_path == path {
          // let _ = fn_name;
          uses.extend([
            ident("let"),
            ident("_"),
            punct('='),
            fn_name.clone(),
            punct(';'),
          ]);
        }
      }
    }
    if input.peek_punct(0, b",") {
      input.must_next();
    }
  }
  if uses.is_empty() {
    return Ok(TokenStream::new());
  }
  // const _: () = { USES };
  Ok(
    [
      ident("const"),
      ident("_"),
      punct(':'),
      parens([]),
      punct('='),
      braces(uses),
      punct(';'),
    ]
    .into_iter()
    .collect(),
  )
}

/// Looks up a `#[t(...)]` attribute in an attribute group produced by
/// `#[rules_derive]` (`$container_attributes`, `$variant_attributes` or
/// `$field_attributes`), and expands to one of two token sequences depending on
/// whether it is present:
///
/// ```ignore
/// attr_lookup!(
///   $field_attributes,
///   json::rename,
///   (@name) => (/* present: `@name` is replaced by the attribute's arguments */),
///   () => (/* absent */),
/// )
/// ```
///
/// For `#[t(json::rename(userName))]` the first arm expands with `@name`
/// replaced by `userName`; for an attribute written without arguments
/// (`#[t(json::skip)]`) `@name` is replaced by nothing. The surrounding
/// parentheses of the chosen arm's body are removed. If the attribute occurs
/// more than once, the first occurrence is used.
///
/// `attr_lookup!` may be used wherever a macro call is allowed. Inside
/// [`with_extra_macros!`] it may additionally be used anywhere, e.g. in a
/// `where` clause.
#[proc_macro]
pub fn attr_lookup(item: TokenStream) -> TokenStream {
  render_macro_result(attr_lookup_inner(item))
}

/// Strips `None`-delimited groups, which `macro_rules!` may wrap around
/// substituted fragments.
fn strip_invisible(mut t: TokenTree) -> TokenTree {
  loop {
    match &t {
      TokenTree::Group(g) if g.delimiter() == Delimiter::None => {
        let mut it = g.stream().into_iter();
        match (it.next(), it.next()) {
          (Some(inner), None) => t = inner,
          _ => return t,
        }
      }
      _ => return t,
    }
  }
}

fn path_string(tokens: impl IntoIterator<Item = TokenTree>) -> String {
  let mut path = String::new();
  for t in tokens {
    match strip_invisible(t) {
      TokenTree::Group(g) if g.delimiter() == Delimiter::None => {
        path.push_str(&path_string(g.stream()));
      }
      t => {
        let t = t.to_string();
        path.push_str(t.strip_prefix("r#").unwrap_or(&t));
      }
    }
  }
  path
}

fn attr_lookup_inner(item: TokenStream) -> Result<TokenStream> {
  let mut input = ParseState::new(item.into_iter());
  let usage = &"expected `attr_lookup!($attributes, path, (@name) => (...), () => (...))`";

  // $attributes
  let attrs_span = input.span();
  let TokenTree::Group(attrs) = strip_invisible(input.next()?) else {
    return Err(Msg(attrs_span, usage));
  };
  if !input.peek_punct(0, b",") {
    return input.error(usage);
  }
  input.must_next(); // ','

  // path
  let mut path_tokens = Vec::new();
  while !input.at_end() && !input.peek_punct(0, b",") {
    path_tokens.push(input.must_next());
  }
  let path = path_string(path_tokens);
  if input.peek_punct(0, b",") {
    input.must_next(); // ','
  }

  // The `#[t(...)]` entries: the first element of the attribute group. Also
  // accept the entries group itself.
  let mut attr_elems = attrs.stream().into_iter();
  let entries = match attr_elems.next().map(strip_invisible) {
    Some(TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => g.stream(),
    _ => attrs.stream(),
  };

  // Find `path (args),` among the entries.
  let mut found: Option<TokenStream> = None;
  {
    let mut current_path = Vec::new();
    for t in entries {
      match strip_invisible(t) {
        TokenTree::Group(g) if g.delimiter() == Delimiter::Parenthesis => {
          if found.is_none() && path_string(current_path.drain(..)) == path {
            found = Some(g.stream());
          }
          current_path.clear();
        }
        TokenTree::Punct(p) if p.as_char() == ',' => current_path.clear(),
        t => current_path.push(t),
      }
    }
  }

  // Arms: (pattern) => (body), ...
  while !input.at_end() {
    let pattern_span = input.span();
    let TokenTree::Group(pattern) = input.next()? else {
      return Err(Msg(pattern_span, usage));
    };
    if !input.peek_punct(0, b"=>") {
      return input.error(usage);
    }
    input.must_next(); // '='
    input.must_next(); // '>'
    let body_span = input.span();
    let TokenTree::Group(body) = input.next()? else {
      return Err(Msg(body_span, usage));
    };
    if input.peek_punct(0, b",") {
      input.must_next(); // ','
    }
    let mut pattern = pattern.stream().into_iter();
    match (pattern.next(), pattern.next(), pattern.next()) {
      (None, None, None) => {
        if found.is_none() {
          return Ok(body.stream());
        }
      }
      (Some(TokenTree::Punct(at)), Some(TokenTree::Ident(name)), None) if at.as_char() == '@' => {
        if let Some(args) = &found {
          return Ok(substitute(body.stream(), &name.to_string(), args));
        }
      }
      _ => return Err(Msg(pattern_span, usage)),
    }
  }
  Ok(TokenStream::new())
}

/// Replaces every `@name` in `body` (at any depth) with `value`.
fn substitute(body: TokenStream, name: &str, value: &TokenStream) -> TokenStream {
  let mut result = Vec::new();
  let mut tokens = body.into_iter().peekable();
  while let Some(t) = tokens.next() {
    match t {
      TokenTree::Punct(p)
        if p.as_char() == '@'
          && matches!(tokens.peek(), Some(TokenTree::Ident(i)) if i.to_string() == name) =>
      {
        tokens.next();
        result.extend(value.clone());
      }
      TokenTree::Group(g) => {
        let mut new = Group::new(g.delimiter(), substitute(g.stream(), name, value));
        new.set_span(g.span());
        result.push(TokenTree::Group(new));
      }
      t => result.push(t),
    }
  }
  result.into_iter().collect()
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
/// the source location attached to `foo`. The `foo` must be a single
/// token-tree.
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
pub fn with_spans(item: TokenStream) -> TokenStream { with_extra_macros(item) }

/// Expands the macros `spanned!(...)` (see [`with_spans!`]) and
/// `attr_lookup!(...)` (see [`attr_lookup!`]) anywhere within its input,
/// including positions where Rust does not allow macro calls, such as `where`
/// clauses and `impl` headers.
///
/// Only an unqualified `attr_lookup!(...)` is expanded here; a path-qualified
/// one (`::rules_derive::attr_lookup!(...)`) is left for rustc to expand as an
/// ordinary macro call. The expansion of an `attr_lookup!` is itself scanned
/// for `spanned!` and `attr_lookup!`.
#[proc_macro]
pub fn with_extra_macros(item: TokenStream) -> TokenStream {
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
                      // `Span::join` returns `None` when the two spans are in
                      // different source files; fall back to the first span.
                      inner_span = first
                        .span()
                        .join(last.span())
                        .unwrap_or_else(|| first.span());
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
    if stream.peek_ident(0, "attr_lookup")
      && stream.peek_punct(1, b"!")
      && stream.peek_group(2)
      && !matches!(dst.last(), Some(TokenTree::Punct(p)) if p.as_char() == ':')
    {
      stream.must_next(); // attr_lookup
      stream.must_next(); // !
      let TokenTree::Group(group) = stream.must_next() else {
        unreachable!()
      };
      let expanded = attr_lookup_inner(group.stream())?;
      process_stream(span, ParseState::new(expanded.into_iter()), dst)?;
      continue 'token_loop;
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
