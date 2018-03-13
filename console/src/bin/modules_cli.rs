//! Flipper modules are libraries loaded onto the device which can be executed remotely. This
//! rust module is a means to execute those loaded modules via the command line. The top-level
//! `modules` rust mod fulfills the functionality given by the `$ flipper module` command.
//! Notably, it defines the behavior for the Read-Eval-Print-Loop (REPL).
//!
//! When a use invokes the REPL with `$ flipper module -r`, the console will prompt for input
//! and treat each new line as if it were passed as arguments to the `flipper module` command.
//!
//! For example:
//! ```
//! $ flipper module -r
//! flipper> button read
//! 1
//! flipper> led rgb 100 0 0
//! flipper> quit
//! $
//! ```

use Result;
use flipper::{
    Flipper,
    api::led::Led,
};
use console::CliError;
use clap::{App, AppSettings, Arg, ArgMatches};
use failure::Error;

pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
    App::new("module")
        .alias("modules")
        .settings(&[
            AppSettings::AllowExternalSubcommands,
            AppSettings::ArgRequiredElseHelp,
            AppSettings::DeriveDisplayOrder,
            AppSettings::ColoredHelp,
        ])
        .template(
            "{bin} \n\
             {before-help} \n\n\
             USAGE: \n    {usage} \n\n\
             {all-args}"
        )
        .before_help("Control modules from the command line or use the interactive REPL")
        .about("Control modules from the command line or use the interactive REPL")
        .arg(Arg::with_name("repl")
            .short("r")
            .long("--repl")
            .help("Enter a Read-Eval-Print-Loop for interactive module control")
        )
        .subcommands(vec![
            led::make_subcommand(),
        ])
}

pub fn execute(args: &ArgMatches) -> Result<()> {
    if args.is_present("repl") {
        repl();
        ::std::process::exit(0);
    }

    match args.subcommand() {
        ("led", Some(m)) => led::execute(m),
        (unknown, _) => Err(CliError::UnrecognizedCommand(unknown.to_owned()).into()),
    }
}

/// The Read-Eval-Print-Loop is used for quickly interacting with Flipper
/// modules. The command interface for executing module functions is the
/// same as from the command line - it uses the same argument parser.
pub fn repl() {
    use rustyline::Editor;
    let mut rl = Editor::<()>::new();
    if let Err(_) = rl.load_history("history.txt") {
        println!("No previous history");
    }

    let mut app = make_subcommand().setting(AppSettings::NoBinaryName);

    loop {
        let readline = rl.readline("flipper> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                let args: Vec<&str> = line.split(' ').collect();
                let matches = app.get_matches_from_safe_borrow(args).unwrap();
                println!("Matches object: {:?}", matches);
                if let Err(e) = execute(&matches) {
                    println!("{}", e);
                }
            }
            Err(_) => break,
        }
    }
    rl.save_history("history.txt").unwrap();
}

pub mod led {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("led")
            .about("Onboard RGB LED")
            .arg(Arg::with_name("red")
                .index(1)
                .required(true)
            )
            .arg(Arg::with_name("green")
                .index(2)
                .required(true)
            )
            .arg(Arg::with_name("blue")
                .index(3)
                .required(true)
            )
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        let red = args.value_of("red").unwrap().parse::<u8>().unwrap();
        let green = args.value_of("green").unwrap().parse::<u8>().unwrap();
        let blue = args.value_of("blue").unwrap().parse::<u8>().unwrap();

        let flipper = Flipper::attach().map_err(Error::from)?;
        let led = Led::new(&flipper);
        led.rgb(red, green, blue);
        Ok(())
    }
}
