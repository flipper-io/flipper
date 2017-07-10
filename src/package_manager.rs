use clap::{App, AppSettings, Arg, ArgMatches};
use app::lang_flags;

pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
    vec![
        App::new("init").args(&lang_flags()),
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
            .about("Update module dependencies to the latest versions")
    ]
}

pub fn execute(args: &ArgMatches) {
    match args.subcommand() {
        ("init", Some(m)) => init::execute(m),
        ("new", Some(m)) => new::execute(m),
        ("add", Some(m)) => add::execute(m),
        ("remove", Some(m)) => remove::execute(m),
        ("update", Some(m)) => update::execute(m),
        (unknown, Some(_)) => println!("Unrecognized command: {}", unknown),
        _ => println!("No command")
    }
}

pub mod init {
    use super::*;
    pub fn execute(args: &ArgMatches) {

    }
}
pub mod new {
    use super::*;
    pub fn execute(args: &ArgMatches) {

    }
}
pub mod add {
    use super::*;
    pub fn execute(args: &ArgMatches) {

    }
}
pub mod remove {
    use super::*;
    pub fn execute(args: &ArgMatches) {

    }
}
pub mod update {
    use super::*;
    pub fn execute(args: &ArgMatches) {

    }
}
