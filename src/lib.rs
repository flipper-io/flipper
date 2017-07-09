#[macro_use]
extern crate clap;
extern crate rustyline;
#[macro_use]
extern crate lazy_static;

pub mod app;
pub mod modules;

use clap::ArgMatches;

/// The CLI trait is to be implemented by pub structs in any modules
/// which fulfill functionality of the command line.
pub trait CLI {
    fn eval(&self, matches: &ArgMatches);
}
