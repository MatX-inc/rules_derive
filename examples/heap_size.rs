//! Example of deriving `HeapSize` for a struct.
//! 
//! Provides examples of: 
//! * specializing rules_derive to structs.
//! * accessing struct fields by name, rather than by pattern matching.
//! * reading a `#[t(...)]` attribute (`#[t(heap_size::skip)]`) with
//!   `attr_lookup!`, including in a `where` clause via `with_extra_macros!`,
//!   and declaring it with `declare_used_attributes!`.
use rules_derive::rules_derive;

pub trait HeapSize {
  fn heap_size(&self) -> usize;
}

macro_rules! HeapSize {
  (
    $container_attributes:tt
    $vis:vis /* require a struct */ struct $name:ident (($ty:ty) ($($generics_bindings:tt)*) ($($generics_inner:tt)*) where ($($generics_where:tt)*)) {
      $variant_attributes:tt
      $variant_name:ident $variant:tt {
        $(
          $field_attributes:tt
          $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
        )*
      }
    }
  ) => {
    // Without this, `#[t(heap_size::skip)]` would be reported as unused.
    declare_used_attributes!(field(heap_size::skip));
    ::rules_derive::with_extra_macros!{
      impl $($generics_bindings)* $crate::HeapSize for $ty where
        $($generics_where)*
        $(
          // A skipped field's type need not implement `HeapSize`.
          attr_lookup!(
            $field_attributes,
            heap_size::skip,
            (@args) => (),
            () => (spanned!($fieldty => $fieldty: $crate::HeapSize,)),
          )
        )*
      {
        fn heap_size(&self) -> usize {
          // It's known to be a struct so we can directly access the field names.
          $(
            attr_lookup!($field_attributes, heap_size::skip, (@args) => (0), () => (self.$fieldname.heap_size()))
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

#[rules_derive(HeapSize)]
pub struct WithSkipped {
  counted: Vec<u32>,
  #[t(heap_size::skip)]
  #[allow(dead_code)]
  cache: std::cell::Cell<u64>, // does not implement HeapSize
}

impl HeapSize for u32 {
  fn heap_size(&self) -> usize { 4 }
}

impl<T: HeapSize> HeapSize for Vec<T> {
  fn heap_size(&self) -> usize {
    self.capacity() * std::mem::size_of::<T>() + self.iter().map(HeapSize::heap_size).sum::<usize>()
  }
}

fn main() {
  let x = WithSkipped { counted: Vec::with_capacity(2), cache: Default::default() };
  assert_eq!(x.heap_size(), 8);
}
