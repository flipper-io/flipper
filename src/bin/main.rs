//! The Flipper Console is a utility to help develop Flipper projects.
//! It supports tasks in package management, hardware management, and
//! even interactive execution of modules loaded on Flipper.
//!
//! Flipper has a large tree of subcommands, so we split the
//! responsibility for the subcommands into child rust modules. Each
//! subcommand module has two responsibilities: report the argument
//! structure from its subtree using `make_subcommand` or
//! `make_subcommands`, and define the implementations for those
//! commands using `execute`. Higher-level commands such as `flipper`
//! itself typically only need to interpret each command enough to
//! decide which child module to pass the execution onto.

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
extern crate clap;
extern crate rustyline;
extern crate byteorder;
extern crate libc;
extern crate goblin;
extern crate flipper;
extern crate flipper_console;

mod modules_cli;
mod packages_cli;
mod hardware_cli;
mod bindings_cli;

use std::process;
use flipper_console::errors::*;
use flipper_console as console;
use clap::{App, AppSettings, Arg, ArgMatches};

const ABOUT: &'static str = "flipper: Manage and control Flipper from the command line";

quick_main!(run);
fn run() -> Result<()> {
    execute(&app().get_matches())
}

/// Create Flipper's top-level argument structure and define App settings.
/// Subcommands are built by calling child rust modules which implement
/// those subcommands.
pub fn app() -> App<'static, 'static> {
    App::new("flipper")
        .author(crate_authors!())
        .version(crate_version!())
        .about(ABOUT)
        .max_term_width(100)
        .settings(&[
            AppSettings::AllowExternalSubcommands,
            AppSettings::ArgRequiredElseHelp,
            AppSettings::ColoredHelp,
            AppSettings::DeriveDisplayOrder,
            AppSettings::UnifiedHelpMessage,
        ])
        .subcommand(modules_cli::make_subcommand())
        .subcommands(hardware_cli::make_subcommands())
        .subcommands(packages_cli::make_subcommands())
        .subcommands(bindings_cli::make_subcommands())
}

/// Determine which child rust module is responsible for the command and pass
/// the execution to it. Note that some top-level commands (such as "boot")
/// are implemented by a child module rather than by the `flipper` module
/// itself. Because of this, we have to explicitly pass the name of the matched
/// command to the child module (e.g. the "c" in `(c @ "boot")`).
pub fn execute(args: &ArgMatches) -> Result<()> {
    match args.subcommand() {
        ("module", Some(m)) => modules_cli::execute(m),
        (c @ "boot", Some(m)) => hardware_cli::execute(c, m),
        (c @ "reset", Some(m)) => hardware_cli::execute(c, m),
        (c @ "flash", Some(m)) => hardware_cli::execute(c, m),
        (c @ "install", Some(m)) => hardware_cli::execute(c, m),
        (c @ "deploy", Some(m)) => hardware_cli::execute(c, m),
        (c @ "init", Some(m)) => packages_cli::execute(c, m),
        (c @ "new", Some(m)) => packages_cli::execute(c, m),
        (c @ "remove", Some(m)) => packages_cli::execute(c, m),
        (c @ "update", Some(m)) => packages_cli::execute(c, m),
        (c @ "generate", Some(m)) => packages_cli::execute(c, m),
        (c @ "bind", Some(m)) => bindings_cli::execute(c, m),
        (unknown, _) => { println!("Unknown command at app.rs: {}", unknown); Ok(()) },
    }
}

/// Describes a common set of flags representing the supported language
/// bindings that the console can interact with.
pub fn lang_flags<'a, 'b>() -> Vec<Arg<'a, 'b>> {
    vec![
        Arg::with_name("java").short("J").long("--java"),
        Arg::with_name("javascript").short("j").long("--javascript"),
        Arg::with_name("python").short("p").long("--python"),
        Arg::with_name("objc").short("o").long("--objc"),
        Arg::with_name("swift").short("s").long("--swift"),
        Arg::with_name("rust").short("r").long("--rust"),
    ]
}
