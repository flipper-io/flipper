//! The package manager is a subset of console commands dedicating to managing Flipper projects.
//! These commands assist in creating new projects, adding and managing module dependencies,
//! and generating language bindings for modules.

use clap::{App, AppSettings, Arg, ArgMatches};
use /*flipper*/::lang_flags;

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
        App::new("add")
            .about("Add a new module dependency to this project")
            .usage("flipper add [--save] [--save-dev] <module>")
            .setting(AppSettings::ArgRequiredElseHelp)
            .args(&[
                Arg::with_name("save").short("-b")
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
            ]),
        App::new("remove")
            .alias("rm")
            .about("Remove a module dependency from this project")
            .setting(AppSettings::ArgRequiredElseHelp)
            .arg(Arg::with_name("module")
                .required(true)
                .takes_value(true)
                .value_name("module")
                .help("The module to remove as a dependency")
            ),
        App::new("update")
            .about("Update module dependencies to the latest versions"),
        App::new("generate")
            .alias("gen")
            .about("Generate Flipper language bindings")
            .before_help("Generate bindings for the current-project module, or [module] if given")
            .args(&lang_flags())
            .arg(Arg::with_name("module")
                .takes_value(true)
                .value_name("module")
                .help("The name of the module to generate language bindings for")
            ),
    ]
}

pub fn execute(args: &ArgMatches) {
    match args.subcommand() {
        ("new", Some(m)) => new::execute(m),
        ("init", Some(m)) => new::execute(m),
        ("add", Some(m)) => add::execute(m),
        ("remove", Some(m)) => remove::execute(m),
        ("update", Some(m)) => update::execute(m),
        ("generate", Some(m)) => generate::execute(m),
        (unknown, Some(_)) => println!("Unrecognized PM command: {}", unknown),
        _ => println!("Illegal subcommand")
    }
}

/// Usage: `flipper new <project>`
/// Creates a new Flipper project in `./project`, creating the directory if it doesn't
/// exist, or populating it if it exists and is empty.
pub mod new {
    use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

/// Usage: `flipper add <module>`
/// Adds `module` as a dependency of the current project.
pub mod add {
    use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

/// Usage: `flipper remove <module>` or `flipper rm <module>`
/// Removes `module` as a dependency of the current project.
pub mod remove {
    use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

/// Usage: `flipper update <module>`
/// Updates all project dependencies to their latest (compatible) versions.
pub mod update {
    use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

/// Usage: `flipper generate [LANG]`, where `[LANG]` is one of:
///        --java, --javascript, --python, --objc, --swift, --rust
/// Flipper modules can be executed remotely from a host machine
/// such as a laptop or mobile phone.
pub mod generate {
    use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}
