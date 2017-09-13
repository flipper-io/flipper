//! The hardware manager is in charge of interactions involving the Flipper
//! hardware which isn't remote module execution. This includes booting the
//! board and installing and deploying modules.

use clap::{App, Arg, ArgMatches};

pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
    vec![
        App::new("boot"),
        App::new("flash")
            .about("Flash a new firmware image onto Flipper")
            .arg(Arg::with_name("image")
                .required(true)
                .takes_value(true)),
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
    ]
}

/// Commands managed by the *hardware manager* are top-level commands, meaning
/// that even though they were parsed by the top-level `flipper` command handler,
/// the argument match was not consumed. Hence, we match on solely the command
/// string, then forward the ArgMatches to the implementing rust mod.
pub fn execute(command: &str, args: &ArgMatches) {
    match command {
        "boot" => boot::execute(args),
        "flash" => flash::execute(args),
        "install" => install::execute(args),
        "deploy" => deploy::execute(args),
        unknown => println!("Unrecognized command: {}", unknown)
    }
}

pub mod boot {
    use super::*;
    use std::process::Command;

    // TODO check that `dfu-programmer` exists before attempting to execute it.
    pub fn execute(args: &ArgMatches) {
        Command::new("dfu-programmer")
            .arg("at90usb162")
            .arg("start")
            .spawn()
            .expect("Error booting Flipper");
    }
}

pub mod flash {
    use super::*;
    use flipper::hardware::fdfu;
    use flipper_rust;

    pub fn execute(args: &ArgMatches) {
        let flipper = flipper_rust::attach();
        fdfu::enter_update_mode();
    }
}

pub mod install {
    use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod deploy {
    use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}
