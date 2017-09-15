#[macro_use]
extern crate serde_derive;
extern crate byteorder;
extern crate xmodem;
extern crate serde;
extern crate toml;

extern crate flipper_rust;

use std::result;

pub mod packages;
pub mod hardware;

pub enum Error {
    FileNotFound,
}

pub type Result<T> = result::Result<T, Error>;
