//! The package manager is a subset of console commands dedicating to managing Flipper projects.
//! These commands assist in creating new projects, adding and managing module dependencies,
//! and generating language bindings for modules.

use flipper_console::CliError;
use clap::{App, AppSettings, Arg, ArgMatches};
use failure::Error;
use ::lang_flags;

pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
    let mut subcommands = vec![
        add::make_subcommand(),
        remove::make_subcommand(),
        update::make_subcommand(),
    ];
    subcommands.extend(new::make_subcommands());
    subcommands
}

/// Commands managed by the *package manager* are top-level commands, meaning
/// that even though they were parsed by the top-level `flipper` command handler,
/// the argument match was not consumed. Hence, we match on solely the command
/// string, then forward the ArgMatches to the implementing rust mod.
pub fn execute(command: &str, args: &ArgMatches) -> Result<(), Error> {
    match command {
        "new" => new::execute(args),
        "init" => new::execute(args),
        "add" => add::execute(args),
        "remove" => remove::execute(args),
        "update" => update::execute(args),
        unknown => Err(CliError::UnrecognizedCommand(unknown.to_owned()).into()),
    }
}

/// Usage: `flipper new <project>`
/// Creates a new Flipper project in `./project`, creating the directory if it doesn't
/// exist, or populating it if it exists and is empty.
pub mod new {
    use super::*;

    pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
        vec![
            App::new("init")
                .about("Create a new Flipper project in the current directory")
                .args(&lang_flags()),
            App::new("new")
                .about("Create a new Flipper project with the given name")
                .args(&lang_flags())
                .arg(Arg::with_name("project")
                    .required(true)
                    .takes_value(true)
                    .value_name("project")
                    .help("The name of the new project")),
        ]
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        unimplemented!();
    }
}

/// Usage: `flipper add <module>`
/// Adds `module` as a dependency of the current project.
pub mod add {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("add")
            .about("Add a new module dependency to this project")
            .usage("flipper add [--save] [--save-dev] <module>")
            .setting(AppSettings::ArgRequiredElseHelp)
            .args(&[
                Arg::with_name("save")
                    .short("-b")
                    .long("--save")
                    .help("Add as a build dependency"),
                Arg::with_name("save-dev")
                    .short("-D")
                    .long("--save-dev")
                    .help("Add as a dev dependency"),
                Arg::with_name("module")
                    .required(true)
                    .takes_value(true)
                    .value_name("module")
                    .help("The name of a module in the Flipper Module Repository"),
            ])
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        unimplemented!();
    }
}

/// Usage: `flipper remove <module>` or `flipper rm <module>`
/// Removes `module` as a dependency of the current project.
pub mod remove {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("remove")
            .alias("rm")
            .about("Remove a module dependency from this project")
            .setting(AppSettings::ArgRequiredElseHelp)
            .arg(Arg::with_name("module")
                .required(true)
                .takes_value(true)
                .value_name("module")
                .help("The module to remove as a dependency")
            )
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        unimplemented!();
    }
}

/// Usage: `flipper update <module>`
/// Updates all project dependencies to their latest (compatible) versions.
pub mod update {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("update")
            .about("Update module dependencies to the latest versions")
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        unimplemented!();
    }
}
