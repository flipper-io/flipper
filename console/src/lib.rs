//!
//!
//!

#![deny(missing_docs)]
#![deny(missing_debug_implementations)]
#![deny(missing_copy_implementations)]
#![deny(trivial_casts)]
#![deny(trivial_numeric_casts)]
#![deny(unsafe_code)]
#![deny(unstable_features)]
#![deny(unused_import_braces)]
#![deny(unused_qualifications)]

#[macro_use] extern crate serde_derive;
#[macro_use] extern crate failure;
#[macro_use] extern crate derive_fail;
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

/// Defines the errors that may be encountered while parsing and executing
/// commands.
#[derive(Debug, Fail)]
#[fail(display = "Command line error")]
pub enum CliError {
    /// An error that indicates that a command or its arguments was invalid.
    #[fail(display = "flipper: '{}' is not a flipper command. See 'flipper --help'.", _0)]
    UnrecognizedCommand(String),
}
