use rules_derive::rules_derive;

macro_rules! NewtypeFrom {
  (
    ($( ($($attr:tt)*) )*)
    $vis:vis struct $name:ident (($ty:ty) ($($generics_bindings:tt)*) ($($generics_inner:tt)*) where ($($generics_where:tt)*)) {
      $variant_name:ident ($variant_style:ident $($qualified_variant:tt)*) {
          // We require the first field to be the main (nonzero-sized) field.
          $main_fieldvis:vis $main_fieldnameident:ident @ $main_fieldname:tt : $main_fieldty:ty,
          $(
              // We allow any number of zero-sized fields.
              $empty_fieldvis:vis $empty_fieldnameident:ident @ $empty_fieldname:tt : $empty_fieldty:ty,
          )*
      }
    }
  ) => {
    ::rules_derive::with_spans! {
      // We insert an extra `FromType` generic parameter into the impl. We splice
      // in the type's own parameters with `$generics_inner` -- the impl bindings
      // *without* the surrounding `<` and `>` -- so we can place `FromType`
      // alongside them. We put `FromType` first because Rust requires lifetime
      // parameters to precede type and const parameters; if the type had its own
      // lifetimes we'd instead append `FromType` after `$generics_inner`.
      impl <FromType, $($generics_inner)*>
          ::std::convert::From<FromType> for $ty where
        $($generics_where)*
        $main_fieldty: std::convert::From<FromType>,
        $(
          spanned!($empty_fieldty => $empty_fieldty: $crate::ZeroSized,)
        )*
      {
        #[inline]
        fn from(value: FromType) -> Self {
          $($qualified_variant)* {
            $main_fieldname: ::std::convert::From::from(value),
            $(
              $empty_fieldname: $crate::ZeroSized::ZERO_SIZED_VALUE,
            )*
          }
        }
      }
    }
  }
}

pub trait ZeroSized: Copy {
  const ZERO_SIZED_VALUE: Self;
}

impl<T> ZeroSized for std::marker::PhantomData<T> {
  const ZERO_SIZED_VALUE: Self = std::marker::PhantomData;
}

#[rules_derive(NewtypeFrom)]
struct SimpleNewtype(u64);

#[rules_derive(NewtypeFrom)]
struct NewtypeWithZeroSizedFields(u64, std::marker::PhantomData<u8>);

#[rules_derive(NewtypeFrom)]
struct GenericNewtype<T>(u64, std::marker::PhantomData<T>);

fn main() {
  assert_eq!(1u64, SimpleNewtype::from(1u16).0);
  assert_eq!(1u64, NewtypeWithZeroSizedFields::from(1u16).0);
  assert_eq!(1u64, GenericNewtype::<u64>::from(1u16).0);
}
