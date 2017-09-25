use std::ops::Deref;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use clap::{App, AppSettings, Arg, ArgMatches};
use flipper_console as console;
use console::errors::*;
use console::bindings::binary_parser;

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
        .subcommand(generate::make_subcommand())
}

pub fn execute(args: &ArgMatches) -> Result<()> {
    match args.subcommand() {
        ("generate", Some(m)) => generate::execute(m),
        (unknown, _) => bail!("Unrecognized command: {}", unknown),
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

    pub fn execute(args: &ArgMatches) -> Result<()> {

        let filename = args.value_of("file")
            .chain_err(|| "error reading filename from args")?;

        let mut file = File::open(filename)
            .chain_err(|| format!("failed to open file: {}", filename))?;

        binary_parser::parse_elf(&mut file);

        Ok(())
    }
}
