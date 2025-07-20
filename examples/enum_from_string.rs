//! Example of parsing a string to a payloadless enum. Provides examples of:
//! * how to require a subset of types, e.g. only payloadless enums
//! * how to operate on names, not just data.
use rules_derive::rules_derive;

pub trait EnumFromString: Sized {
  fn from_str(x: &str) -> Option<Self>;
}

macro_rules! EnumFromString {
  (
    $attrs:tt
    $vis:vis /* require enum */ enum $name:ident (($ty:ty) ($($generics_bindings:tt)*) where ($($generics_where:tt)*)) {
      $(
        $variant_name:ident (/* require unit variant */ unit $($qualified_variant:tt)*) $(= ($discriminant:expr))? {
          // We deliberately don't accept any fields. These matchers are elided:
          //   $(
          //     $fieldvis:vis $fieldnameident:ident @ $fieldname:tt : $fieldty:ty,
          //   )*
        }
      )*
    }
  ) => {
    impl $($generics_bindings)* $crate::EnumFromString for $ty where $($generics_where)*
    {
      fn from_str(x: &str) -> ::std::option::Option<Self> {
        // We create named constants for each variant name, to allow us to use a `match` expression.
        // We stick the variant names inside a module to avoid name conflicts with `x`.
        mod names {
          $(
            #[allow(non_upper_case_globals)]
            pub const $variant_name: &str = ::std::stringify!($variant_name);
          )*
        }
        match x {
          $(
            names::$variant_name => ::std::option::Option::Some($($qualified_variant)*),
          )*
          _ => ::std::option::Option::None,
        }
      } 
    }
  }
}

#[derive(Debug, PartialEq, Eq)]
#[rules_derive(EnumFromString)]
pub enum Foo {
  Foo,
  Bar,
  Baz,
  Quux,
  // Uncomment next line to see that error attribution is good:
  // Invalid {},
}

fn main() {
  assert_eq!(Some(Foo::Foo), Foo::from_str("Foo"));
  assert_eq!(Some(Foo::Bar), Foo::from_str("Bar"));
  assert_eq!(Some(Foo::Baz), Foo::from_str("Baz"));
  assert_eq!(Some(Foo::Quux), Foo::from_str("Quux"));
  assert_eq!(None, Foo::from_str("bamboozle"));
}
