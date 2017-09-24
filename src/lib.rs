#[macro_use]
extern crate serde_derive;
extern crate byteorder;
extern crate xmodem;
extern crate serde;
extern crate toml;
extern crate goblin;
extern crate gimli;
extern crate object;

extern crate flipper_rust;

pub mod packages;
pub mod hardware;
pub mod bindings;

use std::io;
use std::result;

pub enum CliError {
    IoError(io::Error),
}

pub type Result<T> = result::Result<T, CliError>;
