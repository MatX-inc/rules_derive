//! This example prints the type definition produced by the `rules_derive` macro.
//! 
//! Useful for understanding how rules_derive works.
#![allow(unused)]
use rules_derive::rules_derive;

macro_rules! PrintTypeDefinition {
    (
      $($t:tt)*
    ) => {
        declare_used_attributes!(field(json::rename), variant(json::skip));
        const TYPE_DEFINITION: &str = std::stringify!($($t)*);
    }
}


struct NamedFieldsStruct { x: u8, y: u16 }
struct GenericStruct<T: Clone> where usize: Into<T> { x: T, y: T}
enum NamedFieldsEnum { Variant { x: u8, y: u16 } }
enum TupleEnum { Variant(u8, u16) }

#[rules_derive(PrintTypeDefinition)]
#[rustfmt::skip]
pub enum Foo<T: Clone = u8> where u8: Into<T> { 
    A { #[t(json::rename(y))] x: T },
    /// The B variant.
    #[t(json::skip)]
    B,
    C(u8),
}


fn main() {
    println!("{}", TYPE_DEFINITION);
}