use std::ops::Deref;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use std::io::Error as IoError;
use clap::{App, AppSettings, Arg, ArgMatches};
use failure::Error;
use flipper_console::CliError;
//use console::bindings::binary_parser;

#[derive(Debug, Fail)]
#[fail(display = "Errors that occur while generating bindings")]
enum BindingError {
    /// Indicates that an io::Error occurred involving a given file.
    /// _0: The name of the file involved.
    /// _1: The io::Error with details.
    #[fail(display = "File error for '{}': {}", _0, _1)]
    FileError(String, IoError),
}

pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
    App::new("binding")
        .settings(&[
            AppSettings::AllowExternalSubcommands,
            AppSettings::ArgRequiredElseHelp,
            AppSettings::DeriveDisplayOrder,
            AppSettings::ColoredHelp,
        ])
        .template(
            "{bin} \n\
             USAGE: \n    {usage} \n\n\
             {all-args}"
        )
        .subcommands(vec![
            generate::make_subcommand(),
            dwarf::make_subcommand(),
        ])
}

pub fn execute(args: &ArgMatches) -> Result<(), Error> {
    match args.subcommand() {
        ("generate", Some(m)) => generate::execute(m),
        ("dwarf", Some(m)) => dwarf::execute(m),
        (unknown, _) => Err(CliError::UnrecognizedCommand(unknown.to_owned()).into()),
    }
}

/// Usage: `flipper generate [LANG]`, where `[LANG]` is one of:
///        --java, --javascript, --python, --objc, --swift, --rust
/// Flipper modules can be executed remotely from a host machine
/// such as a laptop or mobile phone.
pub mod generate {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("generate")
            .alias("gen")
            .about("Generate Flipper language bindings")
            .before_help("Generate bindings for the current-project module, or [module] if given")
            .arg(Arg::with_name("file")
                .index(1)
                .takes_value(true)
                .required(true)
                .help("The name of the module to generate language bindings for")
            )
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        // Guaranteed to be safe because "file" is a required argument.
        let filename = args.value_of("file").unwrap();

        let mut file = File::open(filename)
            .map_err(|e| BindingError::FileError(filename.to_owned(), e))?;

//        binary_parser::parse_elf(&mut file).map(|_| ())
        Ok(())
    }
}

pub mod dwarf {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("dwarf")
            .arg(Arg::with_name("file")
                .index(1)
                .takes_value(true)
                .required(true)
            )
    }

    pub fn execute(args: &ArgMatches) -> Result<(), Error> {
        // Guaranteed to be safe because "file" is a required argument.
        let filename = args.value_of("file").unwrap();

        let mut file = File::open(filename)
            .map_err(|e| BindingError::FileError(filename.to_owned(), e))?;

//        binary_parser::parse_dwarf(&mut file)
        Ok(())
    }
}
