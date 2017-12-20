//! Flipper modules can define a public API which becomes remotely executable
//! from host platforms like mobile and desktop machines. These platforms can
//! make use of higher level programming languages. This bindings module serves
//! to automatically generate the language bindings necessary to control Flipper
//! from these high level languages.
//!
//! The tasks necessary to perform binding generation are as follows:
//!
//! 1) Parse a Flipper module and read the signatures of functions in its
//! public API.
//!
//! 2) Store the module API in an intermediary format which can be generically
//! translated into any high-level language API.
//!
//! 3) Pass the API description into binding deserializers which generate
//! high-level language packages which expose a functionally equivalent API
//! whose implementation automatically executes the modules on Flipper.

pub mod meta;
