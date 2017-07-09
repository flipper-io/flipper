//! Flipper modules are libraries loaded onto the device which can be executed remotely. This
//! rust module is a means to execute those loaded modules via the command line. The top-level
//! `modules` rust mod fulfills the functionality given by the `$ flipper module` command.
//! Notably, it defines the behavior for the Read-Eval-Print-Loop (REPL).
//!
//! When a use invokes the REPL with `$ flipper module -r`, the console will prompt for input
//! and treat each new line as if it were passed as arguments to the `flipper module` command.
//!
//! For example:
//! ```
//! $ flipper module -r
//! flipper> button read
//! 1
//! flipper> led rgb 100 0 0
//! flipper> quit
//! $
//! ```

pub use clap::{App, ArgMatches};

pub struct CLI<'a> {
    command: App<'a, 'a>,
}

impl<'a> CLI<'a> {
    pub fn new(command: App<'a, 'a>) -> Self {
        Self { command }
    }
}

impl<'a> ::CLI for CLI<'a> {
    fn eval(&self, matches: &ArgMatches) {
        // If no subcommand was given, enter REPL
        if matches.subcommand.is_none() {
            repl(&self.command);
        }
    }
}

pub fn repl(command: &App) {

}

/// Includes logic for locating custom module implementations
pub mod custom {
    pub use super::*;

    pub struct CLI;

    impl ::CLI for CLI {
        fn eval(&self, matches: &ArgMatches) {}
    }
}

pub mod adc {
    pub use super::*;

    pub struct CLI;

    impl ::CLI for CLI {
        fn eval(&self, matches: &ArgMatches) {}
    }
}
