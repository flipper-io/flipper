//! The Rust API for Flipper's common peripherals.

#[macro_use]
extern crate flipper_core;

pub use flipper_core::Flipper;

/// The home of macro-generated Flipper module bindings.
///
/// These modules are thin wrappers around the generated C-like API. Simple Flipper modules that
/// don't need any manual code written to make them idiomatic to Rust live here. More advanced
/// Flipper modules with a manually-written Rust api can be found in the `api` module.
mod sys;

pub use sys::led;
pub use sys::gpio;

/// The home of manually-written Flipper module bindings.
///
/// For some more advanced Flipper modules, we would like to have an API which is richer than
/// what can be expressed by the simple C-wrapping functions. In the `api` module, we wrap the
/// macro-generated bindings and add more Rust-idiomatic patterns.
mod api;

pub use api::uart0;