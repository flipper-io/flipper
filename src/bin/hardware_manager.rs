//! The hardware manager is in charge of interactions involving the Flipper
//! hardware which isn't remote module execution. This includes booting the
//! board and installing and deploying modules.

use clap::{App, Arg, ArgMatches};

pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
    vec![
        boot::make_subcommand(),
        reset::make_subcommand(),
        flash::make_subcommand(),
        install::make_subcommand(),
        deploy::make_subcommand(),
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

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("boot")
    }

    pub fn execute(args: &ArgMatches) {
        let result = Command::new("dfu-programmer")
            .arg("at90usb162")
            .arg("start")
            .spawn();

        match result {
            Ok(_) => return,
            Err(e) => match e.kind() {
                NotFound => println!("Couldn't find dfu-programmer. Please make sure it's in your path"),
                _ => println!("Encountered unexpected error booting Flipper")
            }
        }
    }
}

pub mod reset {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("reset")
    }

    pub fn execute(args: &ArgMatches) {

    }
}

pub mod flash {
    use super::*;
    use flipper::hardware::fdfu;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("flash")
            .about("Flash a new firmware image onto Flipper")
            .arg(Arg::with_name("image")
                .required(true)
                .takes_value(true))
    }

    pub fn execute(args: &ArgMatches) {
        use flipper::hardware::fdfu;
        if let Some(image) = args.value_of("image") {
            println!("Flipper flash got image: {}", image);
            fdfu::flash(image);
        }
    }
}

pub mod install {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("install")
            .about("Install a Flipper package onto the device (persists on reset)")
            .before_help("Install the current-project package, or [package] if given")
            .arg(Arg::with_name("package")
                .required(false)
                .takes_value(true)
                .value_name("package")
                .help("Specifies a package to install, such as from the repository"))
    }

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod deploy {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("deploy")
            .about("Deploy a Flipper package onto the device (lost on reset)")
            .before_help("Deploy the current-project package, or [package] if given")
            .arg(Arg::with_name("package")
                .required(false)
                .takes_value(true)
                .value_name("package")
                .help("Specify a package to install, such as from the repository"))
    }

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}
