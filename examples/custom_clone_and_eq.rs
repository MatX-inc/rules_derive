//! Demonstrates how to implement deriving for our own custom variants of Clone
//! and Eq.
#![allow(deprecated)] // For testing the #[deprecated] attribut.

use rules_derive::make_ident;
use rules_derive::rules_derive;
use rules_derive::with_spans;

pub trait CustomEq {
  fn custom_eq(&self, other: &Self) -> bool;
}

pub trait CustomClone {
  fn custom_clone(&self) -> Self;
}

macro_rules! CustomEq {
  (
    ($( ($($attr:tt)*) )*)
    $vis:vis $tystyle:ident $name:ident ($ty:ty) $(< ($($generics_bindings:tt)*) >)? where ($($generics_where:tt)*) {
      $(
        $variant_name:ident ($variant_style:ident $($qualified_variant:tt)*) $(= ($discriminant:expr))? {
          $(
            $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
          )*
        }
      )*
    }
  ) => {
    with_spans! {
      impl $(< $($generics_bindings)* >)? $crate::CustomEq for $ty where
        $($generics_where)*
        $(
          $(
            spanned!($fieldty => $fieldty: $crate::CustomEq,)
          )*
        )*
      {
        #[inline]
        fn custom_eq(&self, other: &Self) -> bool {
          match self {
            $(
              $($qualified_variant)* { $($fieldname: ($fieldnameident),)* } => {
                match other {
                  $($qualified_variant)* { $($fieldname: make_ident!($fieldnameident _other),)* } =>
                    $(
                      $crate::CustomEq::custom_eq(
                        $fieldnameident,
                        make_ident!($fieldnameident _other)) &&
                    )*
                    true,
                  #[allow(unreachable_patterns)]
                  _ => false,
                }
              }
            )*
          }
        }
      }
    }
  }
}

macro_rules! CustomClone {
  (
    ($( ($($attr:tt)*) )*)
    $vis:vis $tystyle:ident $name:ident ($ty:ty) $(< ($($generics_bindings:tt)*) >)? where ($($generics_where:tt)*) {
      $(
        $variant_name:ident ($variant_style:ident $($qualified_variant:tt)*) $(= ($discriminant:expr))? {
          $(
            $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
          )*
        }
      )*
    }
  ) => {
    with_spans! {
      impl $(< $($generics_bindings)* >)? $crate::CustomClone for $ty where
        $($generics_where)*
        $(
          $(
            spanned!($fieldty => $fieldty: $crate::CustomClone,)
          )*
        )*
      {
        #[inline]
        fn custom_clone(&self) -> Self {
          match self {
            $(
              $($qualified_variant)* { $($fieldname: ($fieldnameident),)* } =>
                $($qualified_variant)* { $($fieldname: $fieldnameident.custom_clone(),)* },
            )*
          }
        }
      }
    }
  }
}


// The deriving works out of the box with all kinds of exotic types, including
// generics. In fact, we give a "better" type to the generics than what Rust's
// default deriving does, i.e. we constrain the field types rather than the
// generic parameters. (Whether this is truly better is a debated topic in Rust).

#[rules_derive(CustomEq, CustomClone)]
#[repr(transparent)]
pub struct UnitStruct;

#[rules_derive(CustomEq, CustomClone)]
#[repr(transparent)]
pub struct UnitStructWithWhere
where
  u8: Clone;

#[rustfmt::skip]
#[rules_derive(CustomEq, CustomClone)]
#[repr(transparent)]
pub struct UnitStructWithWhereAndComma
where
  u8: Clone,;

#[rules_derive(CustomEq, CustomClone)]
#[allow(dead_code)]
pub struct TupleStruct(u8, pub u16, pub(crate) u8);

#[rules_derive(CustomEq, CustomClone)]
#[allow(dead_code)]
pub struct TupleStructWithWhere(u8, pub u16, pub(crate) u8)
where
  u8: Clone;

#[rules_derive(CustomEq, CustomClone)]
#[allow(dead_code)]
#[rustfmt::skip]
pub struct TupleStructWithWhereAndComma(u8, pub u16, pub(crate) u8)
where
  u8: Clone,;

#[rules_derive(CustomEq, CustomClone)]
#[allow(dead_code)]
pub struct EmptyTupleStruct();

#[allow(dead_code)]
#[rules_derive(CustomEq, CustomClone)]
pub struct NamedStruct {
  #[deprecated]
  x: u8,
  #[cfg(any())]
  pub y: u16,
}

#[allow(dead_code)]
#[rules_derive(CustomEq, CustomClone)]
#[rustfmt::skip]
pub struct NamedStructWithWhere
where
  u8: Clone,
{
  x: u8,
  pub y: u16  // No trailing ,
}

#[allow(dead_code)]
#[rules_derive(CustomEq, CustomClone)]
pub struct EmptyNamedStruct {}

#[rules_derive(CustomEq, CustomClone)]
pub struct GenericStruct<'a, T: Copy, PhantomU, const N: usize>(
  pub T,
  pub std::marker::PhantomData<PhantomU>,
  pub &'a [u8; N],
);

#[rules_derive(CustomEq, CustomClone)]
pub struct GenericStructWithWhere<T: Copy>(pub T)
where
  T: Clone;

#[rules_derive(CustomEq, CustomClone)]
pub enum MixedEnum {
  UnitEnum,
  TupleEnum(u8, u16),
  NamedEnum { a: u8, b: u16 },
}

trait SomeTrait<A, B> {
  type Output;
}

impl SomeTrait<u8, u8> for u8 {
  type Output = isize;
}

const FOUR: &isize = &4;

#[rules_derive(CustomEq, CustomClone)]
#[rustfmt::skip]
#[allow(clippy::precedence, clippy::unnecessary_cast)]
pub enum DiscriminantEnum {
  A,
  B = 4,
  // Exercise some difficult mixed-type-and-expression parsing.
  C = *FOUR as <u8 as SomeTrait<u8, u8>>::Output * 2 << 1,
  D = 4 + 5 << 3,
  // Expression and type that end without a trailing comma.
  E = 5 as isize
}

impl CustomEq for u8 {
  #[inline]
  fn custom_eq(&self, other: &Self) -> bool { self == other }
}

impl CustomClone for u8 {
  #[inline]
  fn custom_clone(&self) -> Self { *self }
}

impl CustomEq for u16 {
  #[inline]
  fn custom_eq(&self, other: &Self) -> bool { self == other }
}

impl CustomClone for u16 {
  #[inline]
  fn custom_clone(&self) -> Self { *self }
}

fn main() {
  assert!(DiscriminantEnum::A
    .custom_clone()
    .custom_eq(&DiscriminantEnum::A));
}
