//! The hardware manager is in charge of interactions involving the Flipper
//! hardware which isn't remote module execution. This includes booting the
//! board and installing and deploying modules.

use std::fs::File;
#[allow(unused_imports)]
use std::io::Read;
use console::CliError;
use clap::{App, Arg, ArgMatches};
use indicatif::{ProgressBar, ProgressStyle};
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
            .arg("atmega32u2")
            .arg("start")
            .spawn()
            .map_err(|_| HardwareError::BootError)?;
        Ok(())
    }
}

pub mod flash {
    use super::*;
    use console::hardware::fdfu::{
        self,
        Progress,
    };

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
        info!("Flipper flash got image: {}", image);

        let firmware: Vec<_> = File::open(image)
            .and_then(|mut f| {
                let mut v = Vec::new();
                f.read_to_end(&mut v)?;
                Ok(v)
            })?;

        let flash_total = firmware.len() / 512;
        let verify_total = firmware.len() / 4;

        let verify = args.is_present("verify");
        let receiver = fdfu::flash(firmware.into(), verify);

        let flash_bar = ProgressBar::new(flash_total as u64);
        flash_bar.set_style(ProgressStyle::default_bar()
            .template("{spinner:.green} {msg:20} [{elapsed_precise}] [{bar:40.cyan/blue}] {percent:>3}% ({eta})")
            .progress_chars("##-"));

        let verify_bar = if !verify { None } else {
            let bar = ProgressBar::new(verify_total as u64);
            bar.set_style(ProgressStyle::default_bar()
                .template("{spinner:.green} {msg:20} [{elapsed_precise}] [{bar:40.cyan/blue}] {percent:>3}% ({eta})")
                .progress_chars("##-"));
            Some(bar)
        };

        loop {
            let progress = receiver.recv().unwrap();
            match progress {
                Progress::UpdateMode => flash_bar.set_message("Entered update mode"),
                Progress::NormalMode => flash_bar.set_message("Entered normal mode"),
                Progress::Applet => flash_bar.set_message("Uploaded copy applet"),
                Progress::Flashing(done) => {
                    flash_bar.set_message("Flashing firmware");
                    flash_bar.set_position(done as u64);
                },
                Progress::FlashComplete => flash_bar.finish_with_message("Firmware flashed"),
                Progress::Verifying(done) => {
                    verify_bar.as_ref().unwrap().set_message("Verifying firmware");
                    verify_bar.as_ref().unwrap().set_position(done as u64);
                },
                Progress::VerifyComplete(errors) => {
                    if errors > 0 {
                        verify_bar.as_ref().unwrap().set_message("Verification failed");
                    } else {
                        verify_bar.as_ref().unwrap().finish();
                    }
                }
                Progress::Failed(e) => Err(e)?,
                Progress::Complete => break,
            }
        }

        Ok(())
    }
}
