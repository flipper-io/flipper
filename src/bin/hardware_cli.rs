//! The hardware manager is in charge of interactions involving the Flipper
//! hardware which isn't remote module execution. This includes booting the
//! board and installing and deploying modules.

use flipper_console::CliError;
use clap::{App, Arg, ArgMatches};
use failure::{Fail, Error};

#[derive(Debug, Fail)]
#[fail(display = "A hardware error occurred")]
enum HardwareError {
    #[fail(display = "DFU programmer not found")]
    DfuProgrammerNotFound,
    #[fail(display = "An unknown error occurred while booting Flipper")]
    UnknownBootError,
}

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
pub fn execute(command: &str, args: &ArgMatches) -> Result<(), Error> {
    match command {
        "boot" => boot::execute(args),
        "flash" => flash::execute(args),
        "install" => install::execute(args),
        "deploy" => deploy::execute(args),
        unknown => Err(CliError::UnrecognizedCommand(unknown.to_owned()).into()),
    }
}

pub mod boot {
    use super::*;
    use std::process::Command;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("boot")
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        Command::new("dfu-programmer")
            .arg("at90usb162")
            .arg("start")
            .spawn()
            .map(|_| ())
            .map_err(|e| match e.kind() {
                NotFound => HardwareError::DfuProgrammerNotFound,
                _ => HardwareError::UnknownBootError,
            }.into())
    }
}

pub mod reset {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("reset")
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        unimplemented!();
    }
}

pub mod flash {
    use super::*;
    use console::hardware::fdfu;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("flash")
            .about("Flash a new firmware image onto Flipper")
            .arg(Arg::with_name("image")
                .required(true)
                .takes_value(true))
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        use console::hardware::fdfu;

        // This is safe because "image" is a required argument.
        let image = args.value_of("image").unwrap();
        println!("Flipper flash got image: {}", image);
        fdfu::flash(image)
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

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
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

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        unimplemented!();
    }
}
