use std::fs;
use std::fs::File;
use std::path::PathBuf;
#[allow(unused_imports)] use std::io::Read;
use std::io::Error as IoError;
use clap::{App, AppSettings, Arg, ArgMatches};
use failure::Error;
use console::CliError;
use console::bindings;

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
    App::new("generate")
        .settings(&[
            AppSettings::ArgRequiredElseHelp,
            AppSettings::DeriveDisplayOrder,
            AppSettings::ColoredHelp,
        ])
        .alias("gen")
        .about("Generate Flipper language bindings")
        .before_help("Generate bindings for the specified module")
        .arg(Arg::with_name("file")
            .index(1)
            .takes_value(true)
            .required(true)
            .help("The module file to generate language bindings for"))
        .arg(Arg::with_name("output")
            .long("--output")
            .short("-o")
            .takes_value(true)
            .help("The output directory for the bindings"))
}

pub fn execute(args: &ArgMatches) -> Result<(), Error> {
    // Guaranteed to be safe because these are required arguments.
    let filename = args.value_of("file").unwrap();

    let mut file = File::open(filename)
        .map_err(|e| BindingError::FileError(filename.to_owned(), e))?;

    let path = PathBuf::from(args.value_of("output").unwrap_or("./"));
    fs::create_dir_all(&path);

    let module_binary = {
        let mut v = Vec::new();
        let _ = file.read_to_end(&mut v);
        v
    };

    let modules = bindings::Module::parse(&module_binary)?;
    for module in modules.iter() {
        let mut module_name = module.name.clone();
        module_name.push_str(".c");
        let mut path = PathBuf::from(&path);
        path.push(&module_name);

        let mut out = File::create(&path)?;
        bindings::generators::c::generate_module(module, &mut out)?;
    }

    Ok(())
}
