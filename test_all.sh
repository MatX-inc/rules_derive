#!/usr/bin/env bash
set -e

cargo run --example custom_clone_and_eq
cargo run --example binary_serialization
cargo run --example enum_from_string
cargo run --example heap_size
cargo run --example print_type_definition
cargo +nightly run --example custom_clone_and_eq
cargo +nightly run --example binary_serialization
cargo +nightly run --example enum_from_string
cargo +nightly run --example heap_size
cargo +nightly run --example print_type_definition
echo PASS