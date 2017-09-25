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
        bind::make_subcommand(),
    ]
}

pub fn execute(command: &str, args: &ArgMatches) -> Result<()> {
    match command {
        "bind" => bind::execute(args),
        _ => { println!("Unrecognized command!"); Ok(()) },
    }
}

pub mod bind {
    use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("bind")
            .arg(Arg::with_name("file")
                .index(1)
                .required(true)
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
