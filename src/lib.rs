#![recursion_limit = "1024"]

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

pub mod errors {
    error_chain!{}
}