//! Generate Flipper language binding implementations.
//!
//! The `bindings` mod primarily parses and stores function
//! metadata from Flipper binaries. This mod takes that
//! metadata and generates host implementations that remotely
//! execute the module functions.

pub mod c;

#[derive(Debug, Fail)]
pub enum GeneratorError {
    #[fail(display = "failed to generate C binding: {}", _0)]
    CRenderError(String),
}
