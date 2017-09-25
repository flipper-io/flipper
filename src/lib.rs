//!
//!
//!

#![recursion_limit = "1024"]

#![deny(missing_docs)]
#![deny(missing_debug_implementations)]
#![deny(missing_copy_implementations)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unsafe_code)]
#![deny(unstable_features)]
#![deny(unused_import_braces)]
#![deny(unused_qualifications)]

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate serde_derive;
extern crate byteorder;
extern crate xmodem;
extern crate serde;
extern crate toml;
extern crate goblin;
extern crate gimli;
extern crate object;

extern crate flipper;

pub mod packages;
pub mod hardware;
pub mod bindings;

/// A boilerplate error handling module created using error_chain.
pub mod errors {
    error_chain!{}
}