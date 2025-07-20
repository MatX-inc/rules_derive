use proc_macro::token_stream;
use proc_macro::Delimiter;
use proc_macro::Group;
use proc_macro::Ident;
use proc_macro::Literal;
use proc_macro::Punct;
use proc_macro::Span;
use proc_macro::TokenStream;
use proc_macro::TokenTree;

/// A wrapper around an iterator that allows peeking at the next 3 elements
/// without consuming them.
pub(crate) struct ParseState {
  iter: token_stream::IntoIter,
  pub(crate) peeks: [Option<TokenTree>; 3],
  last_span: Span,
}

impl ParseState {
  pub(crate) fn new(mut iter: token_stream::IntoIter) -> Self {
    let peeks = [iter.next(), iter.next(), iter.next()];
    let last_span = peeks[2]
      .as_ref()
      .or(peeks[1].as_ref())
      .or(peeks[0].as_ref())
      .map(|t| t.span())
      .unwrap_or(Span::call_site());
    Self {
      iter,
      peeks,
      last_span,
    }
  }

  pub(crate) fn next(&mut self) -> Result<TokenTree> {
    let result = self.peeks[0].take();
    self.peeks[0] = self.peeks[1].take();
    self.peeks[1] = self.peeks[2].take();
    self.peeks[2] = self.iter.next();
    self.last_span = self.peeks[2]
      .as_ref()
      .map(|t| t.span())
      .unwrap_or(self.last_span);
    result.ok_or(Msg(self.last_span, &"expected more input"))
  }

  pub(crate) fn must_next(&mut self) -> TokenTree {
    self
      .next()
      .expect("internal error: should have already peeked")
  }

  pub(crate) fn copy(&mut self, n: usize, result: &mut Vec<TokenTree>) -> Result<()> {
    for _ in 0..n {
      result.push(self.next()?);
    }
    Ok(())
  }

  /// Returns a reference to the next element without consuming it.
  pub(crate) fn peek(&self, n: usize) -> Result<&TokenTree> {
    self.peeks[n]
      .as_ref()
      .ok_or(Msg(self.last_span, &"expected more input"))
  }

  /// Returns an error with span of the next token.
  pub(crate) fn error<T>(&self, msg: &'static &'static str) -> Result<T> {
    Err(Msg(self.span(), msg))
  }

  pub(crate) fn span(&self) -> Span {
    self.peeks[0]
    .as_ref()
    .map(|t| t.span())
    .unwrap_or(self.last_span)
  }

  pub(crate) fn peek_ident(&self, n: usize, expected_ident: &str) -> bool {
    self.peek_idents(n, &[expected_ident])
  }

  pub(crate) fn peek_idents(&self, n: usize, expected_idents: &[&str]) -> bool {
    if let Some(TokenTree::Ident(ident)) = &self.peeks[n] {
      let ident_str = ident.to_string();
      for expected_ident in expected_idents {
        if ident_str == *expected_ident {
          return true;
        }
      }
    }
    false
  }

  pub(crate) fn peek_punct(&self, n: usize, expected: &[u8]) -> bool {
    assert!(n + expected.len() <= self.peeks.len());
    for i in 0..expected.len() {
      if let Some(TokenTree::Punct(punct)) = &self.peeks[i + n] {
        if punct.as_char() == expected[i] as char
          && (i == expected.len() - 1 || punct.spacing() == proc_macro::Spacing::Joint)
        {
          continue;
        }
      }
      return false;
    }
    true
  }

  pub(crate) fn peek_puncts(&self, n: usize, expecteds: &[&[u8]]) -> Option<usize> {
    for (i, expected) in expecteds.iter().enumerate() {
      if self.peek_punct(n, expected) {
        return Some(i);
      }
    }
    None
  }

  pub(crate) fn peek_group(&self, n: usize) -> bool {
    matches!(&self.peeks[n], Some(TokenTree::Group(_)))
  }

  pub(crate) fn peek_block(&self, n: usize) -> bool {
    matches!(&self.peeks[n], Some(TokenTree::Group(group)) if group.delimiter() == proc_macro::Delimiter::Brace)
  }

  pub(crate) fn at_end(&self) -> bool { self.peeks[0].is_none() }
}

pub(crate) fn is_punct(tree: &TokenTree, expected_char: char) -> bool {
  matches!(tree, TokenTree::Punct(punct) if punct.as_char() == expected_char)
}

#[derive(Clone, Copy, Debug)]
pub(crate) struct Msg(pub(crate) Span, pub(crate) &'static &'static str);

pub(crate) type Result<T> = std::result::Result<T, Msg>;

pub(crate) fn render_macro_result(result: Result<TokenStream>) -> TokenStream {
  result.unwrap_or_else(|Msg(span, msg)| {
    let spanned = |mut t: TokenTree| {
      t.set_span(span);
      t
    };
    [
      spanned(punct_joint(':')),
      spanned(punct(':')),
      spanned(ident("core")),
      spanned(punct_joint(':')),
      spanned(punct(':')),
      spanned(ident("compile_error")),
      spanned(punct('!')),
      spanned(parens([spanned(string(msg))])),
      spanned(punct(';')),
    ]
    .into_iter()
    .collect()
  })
}

pub(crate) fn with_span(s: Span, mut t: TokenTree) -> TokenTree {
  t.set_span(s);
  t
}
pub(crate) fn ident(s: &str) -> TokenTree {
  TokenTree::Ident(Ident::new(s, proc_macro::Span::call_site()))
}
pub(crate) fn string(s: &str) -> TokenTree { TokenTree::Literal(Literal::string(s)) }
pub(crate) fn parens(v: impl IntoIterator<Item = TokenTree>) -> TokenTree {
  TokenTree::Group(Group::new(Delimiter::Parenthesis, v.into_iter().collect()))
}
pub(crate) fn brackets(v: impl IntoIterator<Item = TokenTree>) -> TokenTree {
  TokenTree::Group(Group::new(Delimiter::Bracket, v.into_iter().collect()))
}
pub(crate) fn braces(v: impl IntoIterator<Item = TokenTree>) -> TokenTree {
  TokenTree::Group(Group::new(Delimiter::Brace, v.into_iter().collect()))
}
pub(crate) fn punct(c: char) -> TokenTree {
  TokenTree::Punct(Punct::new(c, proc_macro::Spacing::Alone))
}
pub(crate) fn punct_joint(c: char) -> TokenTree {
  TokenTree::Punct(Punct::new(c, proc_macro::Spacing::Joint))
}

pub(crate) fn parse_item_attributes(
  input: &mut ParseState,
  result: &mut Vec<TokenTree>,
) -> Result<TokenStream> {
  // OuterAttribute*
  let mut rules_derive_args = None;
  while input.peek_punct(0, b"#") {
    // OuterAttribute → # [ Attr ]
    let pound = input.must_next(); // '#'
    let TokenTree::Group(brackets) = input.next()? else {
      return input.error(&"Expected [...]");
    };
    let mut inner_stream = ParseState::new(brackets.stream().into_iter());
    if rules_derive_args.is_none() && inner_stream.peek_ident(0, "__rules_derive") {
      inner_stream.must_next();
      let args = inner_stream.next()?;
      let TokenTree::Group(args) = args else {
        return input.error(&"Expected (...)");
      };
      rules_derive_args = Some(args.stream());
      // Drop this #[__rules_derive(...)] attribute.
    } else {
      result.push(parens([pound, TokenTree::Group(brackets)]));
    }
  }
  let Some(rules_derive_args) = rules_derive_args else {
    return input.error(&"Could not find internal #[__rules_derive(...)] attribute");
  };
  Ok(rules_derive_args)
}

pub(crate) fn parse_outer_attributes(
  input: &mut ParseState,
  result: &mut Vec<TokenTree>,
) -> Result<()> {
  // OuterAttribute*
  // OuterAttribute → # [ Attr ]
  while input.peek_punct(0, b"#") {
    input.copy(2, result)?; // # [ Attr ]
  }
  Ok(())
}

pub(crate) fn parse_visibility(input: &mut ParseState, result: &mut Vec<TokenTree>) {
  // Parses `Visibility?`.
  //
  // Visibility →
  //     pub
  //   | pub ( crate )
  //   | pub ( self )
  //   | pub ( super )
  //   | pub ( in SimplePath )
  if input.peek_ident(0, "pub") {
    result.push(input.must_next()); // 'pub'
    if let Some(TokenTree::Group(group)) = &input.peeks[0] {
      if group.delimiter() == proc_macro::Delimiter::Parenthesis {
        result.push(input.must_next()); // '(...)'
      }
    }
  }
}

pub(crate) fn parse_where_clause(
  input: &mut ParseState,
  result: &mut Vec<TokenTree>,
) -> Result<()> {
  if input.peek_ident(0, "where") {
    input.must_next(); // 'where'
    let result_len = result.len();
    parse_typelike(input, result, |input| {
      match &input.peeks[0] {
        Some(TokenTree::Group(group)) => {
          if group.delimiter() == proc_macro::Delimiter::Brace {
            return true;
          }
        }
        Some(TokenTree::Punct(punct)) => {
          if punct.as_char() == ';' {
            return true;
          }
        }
        _ => {}
      }
      false
    })?;
    // Ensure generics_where can be concatenated with other where clauses. If it's
    // nonempty, it must end in a comma.
    if result.len() > result_len && !is_punct(&result[result.len() - 1], ',') {
      result.push(punct(','));
    }
  }
  Ok(())
}

/// Parse `GenericParams?`. We want to produce the following token streams:
///  1) `type_tokens`: Generics without bounds or defaults, for instantiating
///     the type itself.
///  2) `generics_bindings`: Generics without defaults, for declaring generics
///     in impls.
pub(crate) fn parse_generics(
  input: &mut ParseState,
  name: &TokenTree,
) -> Result<(Vec<TokenTree>, Vec<TokenTree>)> {
  let mut type_tokens: Vec<TokenTree> = vec![name.clone()];

  let mut generics_bindings: Vec<TokenTree> = Vec::new();
  if input.peek_punct(0, b"<") {
    // GenericParams → < ( GenericParam ( , GenericParam )* ,? )? >
    let open_lt = input.next()?; // '<'
    type_tokens.push(open_lt.clone());
    generics_bindings.push(open_lt);

    loop {
      if input.peek_punct(0, b">") {
        let close_gt = input.next()?; // '>'
        type_tokens.push(close_gt.clone());
        generics_bindings.push(close_gt);
        break;
      }

      // GenericParam → OuterAttribute* ( LifetimeParam | TypeParam | ConstParam )
      {
        let mut unused_attrs = Vec::new();
        parse_outer_attributes(input, &mut unused_attrs)?;
      }
      let param_start = input.next()?;
      match &param_start {
        TokenTree::Ident(ident) => {
          if ident.to_string() == "const" {
            // ConstParam →
            //   const IDENTIFIER : Type
            //     ( = BlockExpression | IDENTIFIER | -? LiteralExpression )?

            generics_bindings.push(param_start); // const
            let const_ident = input.next()?; // IDENTIFIER
            type_tokens.push(const_ident.clone()); // IDENTIFIER
            generics_bindings.push(const_ident); // IDENTIFIER
            generics_bindings.push(input.next()?); // :

            // Type
            parse_typelike(input, &mut generics_bindings, |input| {
              input.peek_puncts(0, &[b"=", b",", b">"]).is_some()
            })?;
            if input.peek_punct(0, b"=") {
              input.next()?; // '='
              let const_value = input.next()?;
              if is_punct(&const_value, '-') {
                input.next()?; // LITERAL
              }
            }
          } else {
            // TypeParam → IDENTIFIER ( : TypeParamBounds? )? ( = Type )?
            type_tokens.push(param_start.clone()); // IDENTIFIER
            generics_bindings.push(param_start); // IDENTIFIER

            if input.peek_punct(0, b":") {
              generics_bindings.push(input.next()?); // ':'
              parse_typelike(input, &mut generics_bindings, |input| {
                input.peek_puncts(0, &[b",", b">", b"="]).is_some()
              })?;
            }
            if input.peek_punct(0, b"=") {
              input.next()?; // '='
              let mut unused = Vec::new();
              // Type
              parse_typelike(input, &mut unused, |input| {
                input.peek_puncts(0, &[b",", b">"]).is_some()
              })?;
            }
          }
        }
        TokenTree::Punct(tick) => {
          if tick.as_char() != '\'' {
            return input.error(&"expected lifetime parameter");
          }
          // LifetimeParam → Lifetime ( : LifetimeBounds )?
          // Lifetime →
          //     LIFETIME_OR_LABEL
          //   | 'static
          //   | '_
          // LIFETIME_OR_LABEL →
          //     ' NON_KEYWORD_IDENTIFIER not immediately followed by '
          //   | RAW_LIFETIME
          // RAW_LIFETIME →
          //   'r# IDENTIFIER_OR_KEYWORD except crate, self, super, Self and not
          // immediately followed by '
          type_tokens.push(param_start.clone()); // '
          generics_bindings.push(param_start); // '
          let lifetime = input.next()?; // identifier or static or _
          type_tokens.push(lifetime.clone());
          generics_bindings.push(lifetime);
          if input.peek_punct(0, b":") {
            // LifetimeBounds → ( Lifetime + )* Lifetime?
            generics_bindings.push(input.next()?); // ':'
            loop {
              generics_bindings.push(input.next()?); // '
              generics_bindings.push(input.next()?); // identifier or static or _
              if input.peek_punct(0, b"+") {
                generics_bindings.push(input.next()?); // '+'
              } else {
                break;
              }
            }
          }
        }
        _ => {
          return input.error(&"parse error in generics");
        }
      }
      if input.peek_punct(0, b",") {
        let comma = input.next()?; // ','
        generics_bindings.push(comma.clone());
        type_tokens.push(comma);
      }
    }
  }
  Ok((type_tokens, generics_bindings))
}

pub(crate) fn parse_named_fields(input: TokenStream) -> Result<Vec<TokenTree>> {
  // Input format:
  //
  // StructFields → StructField ( , StructField )* ,?
  // StructField → OuterAttribute* Visibility? IDENTIFIER : Type
  //
  // Output format:
  //
  // $(
  //   $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
  // )*
  let mut input = ParseState::new(input.into_iter());
  let mut result = Vec::new();
  while !input.at_end() {
    // TODO: attrs.
    skip_outer_attributes(&mut input)?;
    parse_visibility(&mut input, &mut result);
    let field_name = input.next()?; // IDENTIFIER
    let field_name_ident = Ident::new(&format!("field__{}", field_name), Span::call_site());
    result.push(TokenTree::Ident(field_name_ident)); // $fieldnameident:ident
    result.push(punct('@')); // @
    result.push(field_name); // $fieldname:tt
    result.push(input.next()?); // :

    // Type
    parse_typelike(&mut input, &mut result, |input| {
      input.at_end() || input.peek_punct(0, b",")
    })?;
    result.push(punct(',')); // ,

    if input.peek_punct(0, b",") {
      input.next().unwrap(); // ','
      continue;
    }
    break;
  }
  Ok(result)
}

pub(crate) fn parse_unnamed_fields(input: TokenStream) -> Result<Vec<TokenTree>> {
  // Input format:
  //
  // TupleFields → TupleField ( , TupleField )* ,?
  // TupleField → OuterAttribute* Visibility? Type
  //
  // Output format:
  //
  // $(
  //   $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
  // )*
  let mut input = ParseState::new(input.into_iter());
  let mut result = Vec::new();
  let mut field_index = 0usize;
  while !input.at_end() {
    // TODO: attrs.
    skip_outer_attributes(&mut input)?;
    parse_visibility(&mut input, &mut result);
    let field_name_ident = Ident::new(&format!("field__{}", field_index), Span::call_site());
    result.push(TokenTree::Ident(field_name_ident)); // $fieldnameident:ident
    result.push(punct('@')); // @
    result.push(TokenTree::Literal(Literal::usize_unsuffixed(field_index))); // $fieldname:tt
    result.push(punct(':')); // :

    // Type
    parse_typelike(&mut input, &mut result, |input| {
      input.at_end() || input.peek_punct(0, b",")
    })?;
    result.push(punct(',')); // ,

    field_index += 1;
    if input.peek_punct(0, b",") {
      input.next()?; // ','
      continue;
    }
    break;
  }
  Ok(result)
}

/// Parse a balanced sequence of tokens in typelike context until we encounter a
/// specified ender.
pub(crate) fn parse_typelike<F: Fn(&mut ParseState) -> bool>(
  input: &mut ParseState,
  result: &mut Vec<TokenTree>,
  is_end: F,
) -> Result<()> {
  let mut level = 0;
  loop {
    if level == 0 && is_end(input) {
      return Ok(());
    }
    if input.peek_punct(0, b">") {
      level -= 1;
    } else if input.peek_punct(0, b"<") {
      level += 1;
    }
    result.push(input.next()?);
  }
}

/// Approximate parsing of a TypeNoBounds inside an expression. May accept too
/// much, but importantly it returns to expression context at the correct time,
/// i.e. when it encounters an expression operator.
pub(crate) fn parse_type_no_bounds_inside_expression(
  input: &mut ParseState,
  result: &mut Vec<TokenTree>,
) -> Result<()> {
  // We exploit the following structure of TypeNoBounds to simplify the parsing
  // code.
  //
  // TypeNoBounds consists of:
  // 1. Some initial structure, which may use punctuation that is also valid in
  //    expression context, e.g. < * &
  //
  //    We parse this initial structure exactly.
  //
  // 2. Then <=1 TraitBound (or TypePath or SimplePath which are a subset of
  //    TypePath), or BareFunctionType.
  //
  //    The only punctuation allowed in TraitBound is `::`, `::<`, `<`,
  //    `'`, `?`, or punctuation nested inside <...>. We skip over punctuation
  //    nested inside `<...>` by tracking nesting level. Of the remaining
  //    punctuation, only `<` is ambiguous with expression context; we resolve
  //    this ambiguity by observing that `<` is part of a type if and only if it
  //    follows either `::` or an identifier.
  //
  //    In BareFunctionType no punctuation is allowed before the `->` symbol
  //    (if it exists). After the `->` symbol we return back to the beginning
  //    of the parse_type_no_bounds_inside_expression loop, where all punctuation
  //    is allowed again.
  //
  // 3. Then <=1 `!{}` / `![]` / `!()`. Parsing of `!` in this context is
  //    unambiguous because `!` is not an infix expression operator, so if it
  //    occurs in this position it must be a macro invocation.

  'type_no_bounds: loop {
    // Step 1. Parse initial structure exactly. This is a loop that strips off
    // prefixes like `*mut`, `&mut`, etc.
    let t = input.peek(0)?;
    if let TokenTree::Punct(punct) = t {
      match punct.as_char() {
        // NeverType → !
        // InferredType → _
        '!' | '_' => {
          input.copy(1, result)?; // ! or _
          return Ok(());
        }
        // RawPointerType → * ( mut | const ) TypeNoBounds
        '*' => {
          input.copy(2, result)?; // * ( mut | const )
          continue 'type_no_bounds;
        }
        // ReferenceType → & Lifetime? mut? TypeNoBounds
        '&' => {
          input.copy(1, result)?; // &
          if input.peek_punct(0, b"'") {
            input.copy(2, result)?; // ' IDENTIFIER
          }
          if input.peek_ident(0, "mut") {
            input.copy(1, result)?; // mut
          }
          continue 'type_no_bounds;
        }
        // QualifiedPathInType → QualifiedPathType ( :: TypePathSegment )+
        // QualifiedPathType → < Type ( as TypePath )? >
        '<' => {
          input.copy(1, result)?; // <
          parse_typelike(input, result, |input| input.peek_punct(0, b">"))?;
          input.copy(1, result)?; // >

          // fall through to TraitBound/TypePath (part 2).
        }
        _ => {}
      }
    }
    // Step 2. Parse TraitBound / BareFunctionType approximately, using
    // <...> nesting-level and `->` detection.
    let mut level = 0;
    if input.peek_punct(0, b"<") {
      level = 1;
      input.copy(1, result)?;
    }
    'trait_bound: loop {
      if level > 0 {
        let t = input.next()?;
        if let TokenTree::Punct(punct) = &t {
          let c = punct.as_char();
          if c == '>' {
            level -= 1;
          } else if c == '<' {
            level += 1;
          }
        }
        result.push(t);
      } else {
        // `::<` a generic opener
        if input.peek_punct(0, b"::<") {
          input.copy(3, result)?; // ::<
          level += 1;
          continue 'trait_bound;
        } else if input.peek_ident(0, "as") {
          // `as` is the only identifier that terminates a type.
          break 'trait_bound;
        }
        if matches!(&input.peeks[0], Some(TokenTree::Ident(_))) && input.peek_punct(1, b"<") {
          // `IDENTIFIER<` is a generic opener, so long as IDENTIFIER is not `as`,
          // which was checked above.
          input.copy(2, result)?; // IDENTIFIER<
          level += 1;
          continue 'trait_bound;
        } else if input.peek_punct(0, b"->") {
          // `->` returns us back to TypeNoBounds context.
          input.copy(2, result)?; // ->
          continue 'type_no_bounds;
        } else if let Some(TokenTree::Punct(punct)) = &input.peeks[0] {
          // Punctuation that may occur in TraitBound at this point:
          //   ? :: $ !
          //
          // The infix Expression operators are:
          //   as * / % + - << >> & ^ | == != < > <= >= && || .. ..=
          //   = += -= *= /= %= &= |= ^= <<= >>=
          //
          // The only character that occurs in both sets is `!`. So,
          // check for punctuation, resolve ambiguity if it's `!`, and
          // otherwise figure out whether to terminate the type based
          // on the first punctuation character.
          let c = punct.as_char();
          if c == '!' {
            if !input.peek_punct(0, b"!=") {
              // Step 3. Macro invocation:  `!(...)` or `![...]` or `!{...}`
              input.copy(2, result)?; // ! (...)
            }
            break 'type_no_bounds;
          } else if !(c == '?' || c == ':' || c == '$') {
            // Anything other than the few operators that are allowed in
            // TraitBound terminates the type.
            break 'type_no_bounds;
          }
        }
        // No special processing, just copy the token across
        if input.copy(1, result).is_err() {
          break 'type_no_bounds;
        }
      }
    }
  }
  Ok(())
}

/// Approximate parsing of Expression, up to a ',' ender.
pub(crate) fn parse_expression(input: &mut ParseState, result: &mut Vec<TokenTree>) -> Result<()> {
  // We're parsing Expression from https://doc.rust-lang.org/stable/reference/expressions.html.
  //
  // In a different formulation from there, we treat Expression as a sequence of
  // non-operator Expression trees, interspersed with binary and unary
  // operators. After any binary or unary operator, a new Expression starts.
  'start_of_expression: loop {
    // Here we're at the beginning of an Expression.
    parse_outer_attributes(input, result)?;
    // If we encounter `<` then we
    // enter type context.
    if input.peek_punct(0, b"<") {
      // QualifiedPathInExpression → QualifiedPathType ( :: PathExprSegment )+
      // QualifiedPathType → < Type ( as TypePath )? >
      input.copy(1, result)?; // <
      parse_typelike(input, result, |input| input.peek_punct(0, b">"))?;
      input.copy(1, result)?; // >
    } else if input.peek_punct(0, b"|") || input.peek_idents(0, &["async", "move"]) {
      // ClosureExpression →
      //   async?
      //   move?
      //   ( `||` | `|` ClosureParameters? `|` )
      //   ( Expression | -> TypeNoBounds BlockExpression )
      if input.peek_ident(0, "async") {
        input.copy(1, result)?; // async
      }
      if input.peek_ident(0, "move") {
        input.copy(1, result)?; // move
      }
      input.copy(1, result)?; // |
      parse_typelike(input, result, |input| input.peek_punct(0, b"|"))?;
      input.copy(1, result)?; // |
      if input.peek_punct(0, b"->") {
        // -> TypeNoBounds BlockExpression
        input.copy(2, result)?; // ->
        parse_typelike(input, result, |input| input.peek_block(0))?;
        input.copy(1, result)?; // BlockExpression

      // Fall through to parse the rest of the expression, e.g. an operator.
      } else {
        // Return back to parsing Expression.
        continue 'start_of_expression;
      }
    } else if input.peek_idents(0, &["break", "return"]) {
      // BreakExpression → break LIFETIME_OR_LABEL? Expression?
      // ReturnExpression → return Expression?
      input.copy(1, result)?; // break | return
      if input.peek_punct(0, b"'") {
        input.copy(2, result)?; // ' IDENTIFIER
      }
      // "break" and "return" have lowest precedence. If anything at all follows them,
      // it's back to Expression context. Otherwise, we're done parsing the
      // expression.
      if !input.at_end() && !input.peek_punct(0, b",") {
        continue 'start_of_expression;
      } else {
        return Ok(());
      }
    } else if let Some(i) = input.peek_puncts(0, &[b"&", b"!", b"*", b"-"]) {
      // Unary operators.

      // Only '&' needs special handling, for `&mut`, `&raw const` syntax.
      if i == 0 {
        input.copy(1, result)?; // &
        if input.peek_ident(0, "raw") && input.peek_idents(1, &["const", "mut"]) {
          input.copy(2, result)?; // raw const
        } else if input.peek_ident(0, "mut") {
          input.copy(1, result)?; // mut
        }
      } else {
        input.copy(1, result)?;
      }
      continue 'start_of_expression;
    } else {
      // Otherwise we're in expression context.
      input.copy(1, result)?;
    }
    // Now we're in the middle of an Expression. Scan for `::<` (enters type
    // context) or binary operators (returns to beginning of Expression).
    'middle_of_expression: loop {
      if input.at_end() || input.peek_punct(0, b",") {
        return Ok(());
      } else if input.peek_punct(0, b"::<") {
        input.copy(3, result)?; // ::<
        parse_typelike(input, result, |input| input.peek_punct(0, b">"))?;
        input.copy(1, result)?; // >
        continue 'middle_of_expression;
      } else if input.peek_ident(0, "as") {
        // TypeCastExpression → Expression as TypeNoBounds
        input.copy(1, result)?; // as
        parse_type_no_bounds_inside_expression(input, result)?;
        continue 'middle_of_expression;
      } else if let Some(TokenTree::Punct(_)) = &input.peeks[0] {
        // Expression infix operators, sorted with longer operators first.
        const OPERATORS: &[&[u8]] = &[
          b"*=", b"*", // *
          b"/=", b"/", // /
          b"+=", b"+", // +
          b"-=", b"-", // -
          b"<<=", b"<<", b"<=", b"<", // <
          b">>=", b">>", b">=", b">", // >
          b"&=", b"&&", b"&", // &
          b"^=", b"^", // ^
          b"|=", b"||", b"|", // |
          b"==", b"=",  // =
          b"!=", // !
          b"..=", b"..", // .
        ];
        if let Some(i) = input.peek_puncts(0, OPERATORS) {
          input.copy(OPERATORS[i].len(), result)?;
          continue 'middle_of_expression;
        }
        // .. . ? '
        //
        // Fall through to non-special token.
      }
      input.copy(1, result).unwrap();
    }
  }
}

pub(crate) fn skip_outer_attributes(input: &mut ParseState) -> Result<()> {
  while input.peek_punct(0, b"#") {
    input.next().unwrap(); // '#'
    let Ok(TokenTree::Group(group)) = input.next() else {
      return input.error(&"expected #[...]");
    };
    if group.delimiter() != proc_macro::Delimiter::Bracket {
      return input.error(&"expected #[...]");
    }
  }
  Ok(())
}
