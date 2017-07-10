use clap::{App, AppSettings, Arg, ArgMatches};

use app::{lang_flags};

pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
    vec![
        App::new("boot"),
        App::new("install")
            .about("Install a Flipper package onto the device (persists on reset)")
            .before_help("Install the current-project package, or [package] if given")
            .arg(Arg::with_name("package")
                .required(false)
                .takes_value(true)
                .value_name("package")
                .help("Specifies a package to install, such as from the repository")),
        App::new("deploy")
            .about("Deploy a Flipper package onto the device (lost on reset)")
            .before_help("Deploy the current-project package, or [package] if given")
            .arg(Arg::with_name("package")
                .required(false)
                .takes_value(true)
                .value_name("package")
                .help("Specify a package to install, such as from the repository")),
        App::new("generate")
            .alias("gen")
            .about("Generate Flipper language bindings")
            .before_help("Generate bindings for the current-project module, or [module] if given")
            .args(&lang_flags())
            .arg(Arg::with_name("module")
                .takes_value(true)
                .value_name("module")
                .help("The name of the module to generate language bindings for"))
    ]
}

pub fn execute(args: &ArgMatches) {
    match args.subcommand() {
        ("install", Some(m)) => install::execute(m),
        ("deploy", Some(m)) => deploy::execute(m),
        ("generate", Some(m)) => generate::execute(m),
        _ => println!("Unrecognized command"),
    }
}

pub mod install {
    use super::*;
    pub fn execute(args: &ArgMatches) {

    }
}

pub mod deploy {
    use super::*;
    pub fn execute(args: &ArgMatches) {

    }
}

pub mod generate {
    use super::*;
    pub fn execute(args: &ArgMatches) {

    }
}
