//! The hardware manager is in charge of interactions involving the Flipper
//! hardware which isn't remote module execution. This includes booting the
//! board and installing and deploying modules.

use std::fs::File;
#[allow(unused_imports)]
use std::io::Read;
use console::CliError;
use clap::{App, Arg, ArgMatches};
use failure::Error;

#[derive(Debug, Fail)]
#[fail(display = "A hardware error occurred")]
enum HardwareError {
    #[fail(display = "An error occurred while booting Flipper. Please make sure dfu-programmer is installed.")]
    BootError,
}

pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
    vec![
        boot::make_subcommand(),
        flash::make_subcommand(),
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
        unknown => Err(CliError::UnrecognizedCommand(unknown.to_owned()).into()),
    }
}

pub mod boot {
    use super::*;
    use std::process::Command;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("boot")
            .about("Boot Flipper into interactive mode")
    }

    pub fn execute(_: &ArgMatches) -> Result<(), Error> {
        Command::new("dfu-programmer")
            .arg("at90usb162")
            .arg("start")
            .spawn()
            .map_err(|_| HardwareError::BootError)?;
        Ok(())
    }
}

pub mod flash {
    use super::*;
    use console::hardware::fdfu;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("flash")
            .about("Flash a new firmware image onto Flipper")
            .arg(Arg::with_name("verify")
                .long("verify")
                .help("After uploading, verify the firmware was written correctly"))
            .arg(Arg::with_name("image")
                .required(true)
                .takes_value(true))
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        // This is safe because "image" is a required argument.
        let image = args.value_of("image").unwrap();
        println!("Flipper flash got image: {}", image);

        let firmware = File::open(image)
            .and_then(|mut f| {
                let mut v = Vec::new();
                f.read_to_end(&mut v)?;
                Ok(v)
            })?;

        fdfu::flash(&firmware, args.is_present("verify"))
    }
}
