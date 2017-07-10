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

pub use clap::{App, AppSettings, Arg, ArgMatches, SubCommand as Sub};

pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
    App::new("module")
        .alias("modules")
        .settings(&[
            AppSettings::AllowExternalSubcommands,
            AppSettings::ArgRequiredElseHelp,
        ])
        .template(
            "{bin} \n\
             {before-help} \n\n\
             USAGE: \n    {usage} \n\n\
             {all-args}"
        )
        .before_help("Control modules from the command line or use the interactive REPL")
        .arg(Arg::with_name("repl")
            .short("r")
            .long("--repl")
            .help("Enter a Read-Eval-Print-Loop for interactive module control")
        )
        .subcommands(vec![
            App::new("adc").about("Analog to Digital Converter"),
            App::new("button").about("Flipper's onboard button"),
            App::new("dac").about("Digital to Analog Converter"),
            App::new("fs").about("Filesystem"),
            App::new("gpio").about("General-Purpose Input/Output"),
            App::new("i2c").about("Inter-IC (integrated circuit) bus"),
            App::new("led").about("Onboard RGB LED"),
            App::new("pwm").about("Pulse-Width Modulation"),
            App::new("rtc").about("Real-Time Clock"),
            App::new("spi").about("Serial Peripheral Interface"),
            App::new("swd").about("Serial Wire Debug"),
            App::new("temp").about("Temperature"),
            App::new("timer").about("General purpose Timer"),
            App::new("uart0").about("Universal Asynchronous Receive/Transmit, bus 0"),
            App::new("usart").about("Universal Synchronous/Asynchronous Receive/Transmit"),
            App::new("usb").about("Universal Serial Bus"),
            App::new("wdt").about("WatchDog Timer"),
        ])
}

pub fn execute(args: &ArgMatches) {
    if args.is_present("repl") {
        repl();
        ::std::process::exit(0);
    }

    match args.subcommand() {
        ("adc", Some(m)) => adc::execute(m),
        ("button", Some(m)) => button::execute(m),
        ("dac", Some(m)) => dac::execute(m),
        ("fs", Some(m)) => fs::execute(m),
        ("gpio", Some(m)) => gpio::execute(m),
        ("i2c", Some(m)) => i2c::execute(m),
        ("led", Some(m)) => led::execute(m),
        ("pwm", Some(m)) => pwm::execute(m),
        ("rtc", Some(m)) => rtc::execute(m),
        ("spi", Some(m)) => spi::execute(m),
        ("swd", Some(m)) => swd::execute(m),
        ("temp", Some(m)) => temp::execute(m),
        ("timer", Some(m)) => timer::execute(m),
        ("uart0", Some(m)) => uart0::execute(m),
        ("usart", Some(m)) => usart::execute(m),
        ("usb", Some(m)) => usb::execute(m),
        ("wdt", Some(m)) => wdt::execute(m),
        (unknown, Some(_)) => println!("Unrecognized module: {}", unknown),
        _ => println!("Invalid command")
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
                execute(&matches);
            }
            Err(_) => break,
        }
    }
    rl.save_history("history.txt").unwrap();
}

pub mod adc {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod button {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod dac {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod fs {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod gpio {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod i2c {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod led {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod pwm {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod rtc {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod spi {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod swd {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod temp {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod timer {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod uart0 {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod usart {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod usb {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

pub mod wdt {
    pub use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}

/// Includes logic for locating custom module implementations
pub mod custom {
    use super::*;

    pub fn execute(args: &ArgMatches) {
        unimplemented!();
    }
}
