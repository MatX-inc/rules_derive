use rules_derive::rules_derive;

macro_rules! Clone {
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
      impl $($generics_bindings)* ::std::clone::Clone for $ty where
        $($generics_where)*
        $(
          $(
            spanned!($fieldty => $fieldty: ::std::clone::Clone,)
          )*
        )*
      {
        #[inline]
        fn clone(&self) -> Self {
          match self {
            $(
              $($qualified_variant)* { $($fieldname: ($fieldnameident),)* } =>
                $($qualified_variant)* { $($fieldname: $fieldnameident.clone(),)* },
            )*
          }
        }
      }
    }
  }
}

#[rules_derive(Clone)]
pub enum Foo {
    A,
    B { x: u32 },
    C(u8),
}

fn main() {
    let a = Foo::A;
    _ = a.clone();
}