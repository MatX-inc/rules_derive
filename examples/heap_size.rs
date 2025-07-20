//! Example of deriving `HeapSize` for a struct.
//! 
//! Provides examples of: 
//! * specializing rules_derive to structs.
//! * accessing struct fields by name, rather than by pattern matching.
use rules_derive::rules_derive;
use rules_derive::with_spans;

pub trait HeapSize {
  fn heap_size(&self) -> usize;
}

macro_rules! HeapSize {
  (
    ($( ($($attr:tt)*) )*)
    $vis:vis /* require a struct */ struct $name:ident (($ty:ty) ($($generics_bindings:tt)*) where ($($generics_where:tt)*)) {
      $variant_name:ident $variant:tt {
        $(
          $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
        )*
      }
    }
  ) => {
    with_spans!{
      impl $($generics_bindings)* $crate::HeapSize for $ty where
        $($generics_where)*
        $(
          spanned!($fieldty => $fieldty: $crate::HeapSize,)
        )*
      {
        fn heap_size(&self) -> usize {
          // It's known to be a struct so we can directly access the field names.
          $(
            self.$fieldname.heap_size()
            +
          )*
          0
        }
      }
    }
  }
}

#[allow(dead_code)]
#[rules_derive(HeapSize)]
pub struct Unit;

#[allow(dead_code)]
#[rules_derive(HeapSize)]
pub struct Unnamed(u32, u32);

impl HeapSize for u32 {
  fn heap_size(&self) -> usize { 4 }
}

fn main() {}
