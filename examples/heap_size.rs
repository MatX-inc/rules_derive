use rules_derive::rules_derive;
use rules_derive::with_spans;

pub trait HeapSize {
  fn heap_size(&self) -> usize;
}

macro_rules! HeapSize {
  (
    ($( ($($attr:tt)*) )*)
    $vis:vis struct $name:ident (($ty:ty) ($($generics_bindings:tt)*) where ($($generics_where:tt)*)) {
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
          spanned!($fieldnameident => $fieldty: $crate::HeapSize,)
        )*
      {
        fn heap_size(&self) -> usize {
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
