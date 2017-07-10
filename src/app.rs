use clap::{App, AppSettings, Arg, ArgMatches};

use management;
use modules;
use package_manager as pm;

const ABOUT: &'static str = "flipper: Manage and control Flipper from the command line";

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
        .subcommands(management::make_subcommands())
        .subcommands(pm::make_subcommands())
}

pub fn execute(args: &ArgMatches) {
    match args.subcommand() {
        ("module", Some(m)) => modules::execute(m),
        ("boot", Some(m)) => management::execute(m),
        ("install", Some(m)) => management::execute(m),
        ("deploy", Some(m)) => management::execute(m),
        ("generate", Some(m)) => management::execute(m),
        ("init", Some(m)) => pm::execute(m),
        ("new", Some(m)) => pm::execute(m),
        ("remove", Some(m)) => pm::execute(m),
        ("update", Some(m)) => pm::execute(m),
        (unknown, Some(_)) => println!("Unknown command at app.rs: {}", unknown),
        _ => println!("No command matches"),
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
