use std::ops::Deref;
use std::path::PathBuf;
use std::fs::File;
use std::io::Read;
use clap::{App, Arg, ArgMatches};
use flipper_console as console;
use console::errors::*;
use console::bindings::binary_parser;

pub fn make_subcommands<'a, 'b>() -> Vec<App<'a, 'b>> {
    vec![
        generate::make_subcommand(),
    ]
}

pub fn execute(command: &str, args: &ArgMatches) -> Result<()> {
    match command {
        "generate" => generate::execute(args),
        _ => bail!("Unrecognized command!"),
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
            .arg(Arg::with_name("module")
                .takes_value(true)
                .value_name("module")
                .help("The name of the module to generate language bindings for")
            )
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {

        let filename = args.value_of("file")
            .chain_err(|| "error reading filename")?;

        println!("flipper bind got {}", filename);
        let path = PathBuf::from(filename);
        let mut file = File::open(path).unwrap();

        binary_parser::parse_elf(&mut file);

        Ok(())
    }
}
