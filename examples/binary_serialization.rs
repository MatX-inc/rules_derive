//! Example of serializing/deserializing a type to/from Vec<u8>.
//! 
//! Provides examples of:
//! * supporting arbitrary sum-of-products types
//! * how to assign sequential numbers to variants.
use rules_derive::rules_derive;

pub trait BinarySerializable: Sized {
  fn encode(&self, dst: &mut Vec<u8>);
  fn decode(src: &mut &[u8]) -> Result<Self, DecodeError>;
}

#[derive(Debug)]
pub struct DecodeError;

macro_rules! impl_integer {
  ($($t:ty),*) => {
    $(
      impl BinarySerializable for $t {
        #[inline(always)]
        fn encode(&self, dst: &mut Vec<u8>) {
          dst.extend_from_slice(&self.to_le_bytes());
        }

        fn decode(src: &mut &[u8]) -> Result<Self, DecodeError> {
          let Some((chunk, rest)) = src.split_first_chunk() else {
            return Err(DecodeError);
          };
          *src = rest;
          Ok(Self::from_le_bytes(*chunk))
        }
      }
    )*
  }
}
impl_integer!(u8, u16, u32, u64, u128, usize, i8, i16, i32, i64, i128, isize);

macro_rules! BinarySerializable {
  (
    ($( ($($attr:tt)*) )*)
    $vis:vis $tystyle:ident $name:ident (($ty:ty) ($($generics_bindings:tt)*) where ($($generics_where:tt)*)) {
      $(
        $variant_name:ident ($variant_style:ident $($qualified_variant:tt)*) $(= ($discriminant:expr))? {
          $(
            $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
          )*
        }
      )*
    }
  ) => {
    ::rules_derive::with_spans! {
      impl $($generics_bindings)* $crate::BinarySerializable for $ty where
        $($generics_where)*
        $(
          $(
            spanned!($fieldty => $fieldty: $crate::BinarySerializable,)
          )*
        )*
      {
        #[inline]
        fn encode(&self, dst: &mut ::std::vec::Vec<u8>) {
          enum TagNumber {
            $($variant_name,)*
          }
          const NUM_TAGS: usize = 0 $( + $crate::constant_one!($variant_name))*;

          match self {
            $(
              $($qualified_variant)* { $($fieldname: ($fieldnameident),)* } => {
                if NUM_TAGS > 0 { 
                  $crate::BinarySerializable::encode(&(TagNumber::$variant_name as usize), dst);
                }
                $(
                  $crate::BinarySerializable::encode($fieldnameident, dst);
                )*
              }
            )*
          }
        }

        #[inline]
        fn decode(src: &mut &[u8]) -> ::std::result::Result<Self, $crate::DecodeError> {
          mod constants {
            pub enum TagNumber {
              $($variant_name,)*
            }
            // Put all constants in a separate namespace so they don't conflict with `TagNumber`.
            pub mod tag_number {
              $(
                #[allow(non_upper_case_globals)]
                pub const $variant_name: usize = super::TagNumber::$variant_name as usize;
              )*
            }
          }
          const NUM_TAGS: usize = 0 $( + $crate::constant_one!($variant_name))*;

          let tag: usize = if NUM_TAGS > 0 {
            $crate::BinarySerializable::decode(src)?
          } else {
            0
          };
          match tag {
            $(
              constants::tag_number::$variant_name => {
                $(
                  let $fieldnameident = $crate::BinarySerializable::decode(src)?;
                )*
                ::std::result::Result::Ok($($qualified_variant)* { $($fieldname: ($fieldnameident),)* })
              }
            )*
            _ => {
              ::std::result::Result::Err($crate::DecodeError)
            }
          }
        }
      }
    }
  }
}

#[macro_export]
#[doc(hidden)]
macro_rules! constant_one {
  ($t:tt) => { 1 }
}



#[rules_derive(BinarySerializable)]
#[derive(Debug, PartialEq, Eq)]
enum Foo {
  Bar,
  Baz { x: u32, y: u8 },
  Quux(u8),
}

fn main() {
  let mut serial = Vec::new();
  Foo::Baz { x: 4, y: 3 }.encode(&mut serial);
  let mut slice = &serial[..];
  let decoded = Foo::decode(&mut slice).unwrap();
  assert_eq!(decoded, Foo::Baz { x: 4, y: 3 });
}
