# Examples of `rules_derive`

Some example usages of `rules_derive`.

Starting examples:

* Start with [clone.rs](clone.rs). This demonstrates the basic usage.
* To see `rules_derive` internal representation, run [print_type_definition](print_type_definition.rs): `cargo run --example print_type_definition`.

More advanced examples:

* [enum_from_string.rs](enum_from_string.rs). Example of parsing a string to a payloadless enum. Provides examples of:
  * how to require a subset of types, e.g. only payloadless enums
  * how to operate on names, not just data.
* [heap_size.rs](heap_size.rs). Example of deriving `HeapSize` for a struct. Provides examples of: 
  * specializing rules_derive to structs.
  * accessing struct fields by name, rather than by pattern matching.
* [binary_serialization.rs](binary_serialization.rs). Example of serializing/deserializing a type to/from `Vec<u8>`. Provides examples of:
  * how to assign sequential numbers to variants.
* [custom_clone_and_eq.rs](custom_clone_and_eq.rs). Examples for custom `Eq` and `Clone` traits. Used primarily for testing `rules_derive`'s internal parser.
* [newtype_from.rs](newtype_from.rs). Examples of defining an `impl` that has additional generic parameters beyond what the type itself has.

You can test all examples with:

```sh
./test_all.sh
```