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

#[macro_use]
extern crate clap;
extern crate rustyline;
extern crate byteorder;
extern crate libc;
extern crate flipper;
extern crate flipper_rust;

pub mod modules;
pub mod package_manager;
pub mod hardware_manager;

use clap::{App, AppSettings, Arg, ArgMatches};
use package_manager as pm;
use hardware_manager as hw;

const ABOUT: &'static str = "flipper: Manage and control Flipper from the command line";

fn main() {
    execute(&app().get_matches());
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
        .subcommand(modules::make_subcommand())
        .subcommands(hw::make_subcommands())
        .subcommands(pm::make_subcommands())
}

/// Determine which child rust module is responsible for the command and pass
/// the execution to it. Note that some top-level commands (such as "boot")
/// are implemented by a child module rather than by the `flipper` module
/// itself. Because of this, we have to explicitly pass the name of the matched
/// command to the child module (e.g. the "c" in `(c @ "boot")`).
pub fn execute(args: &ArgMatches) {
    match args.subcommand() {
        ("module", Some(m)) => modules::execute(m),
        (c @ "boot", Some(m)) => hw::execute(c, m),
        (c @ "flash", Some(m)) => hw::execute(c, m),
        (c @ "install", Some(m)) => hw::execute(c, m),
        (c @ "deploy", Some(m)) => hw::execute(c, m),
        (c @ "init", Some(m)) => pm::execute(c, m),
        (c @ "new", Some(m)) => pm::execute(c, m),
        (c @ "remove", Some(m)) => pm::execute(c, m),
        (c @ "update", Some(m)) => pm::execute(c, m),
        (c @ "generate", Some(m)) => pm::execute(c, m),
        (unknown, _) => println!("Unknown command at app.rs: {}", unknown),
    }
}

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
