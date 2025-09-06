use rules_derive::rules_derive;

macro_rules! NewtypeFrom {
  (
    ($( ($($attr:tt)*) )*)
    $vis:vis struct $name:ident ($ty:ty) $(< ($($generics_bindings:tt)*) >)? where ($($generics_where:tt)*) {
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
      // We insert the `T: From<$main_fieldty>` generic parameter here. Rust requires that lifetime
      // bindings come first and non-lifetime bindings (types and consts) come after. Given that 
      // `T: From<$main_fieldty>` is a non-lifetime binding, we insert it at the end of all the other
      // bindings. If we were adding a lifetime binding, we'd insert it at the beginning of all the
      // other bindings.
      //
      // Additionally, instead of wrapping the generics in `$(< ... >)?``, we wrap them in `< ... >`.
      // In other words, the impl generics must always be present.
      impl < $($($generics_bindings)*)* 
             FromType
           >
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
struct GenericNewtype<'a, T>(&'a u64, std::marker::PhantomData<T>);

fn main() {
  assert_eq!(1u64, SimpleNewtype::from(1u16).0);
  assert_eq!(1u64, NewtypeWithZeroSizedFields::from(1u16).0);
  assert_eq!(1u64, *GenericNewtype::<'_, u64>::from(&1u64).0);
}