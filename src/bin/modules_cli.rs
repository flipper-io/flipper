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

use flipper;
use flipper_console as console;
use console::errors::*;
use clap::{App, AppSettings, Arg, ArgMatches, SubCommand as Sub};

pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
    App::new("module")
        .alias("modules")
        .settings(&[
            AppSettings::AllowExternalSubcommands,
            AppSettings::ArgRequiredElseHelp,
            AppSettings::DeriveDisplayOrder,
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
            adc::make_subcommand(),
            button::make_subcommand(),
            dac::make_subcommand(),
            fs::make_subcommand(),
            gpio::make_subcommand(),
            i2c::make_subcommand(),
            led::make_subcommand(),
            pwm::make_subcommand(),
            rtc::make_subcommand(),
            spi::make_subcommand(),
            swd::make_subcommand(),
            temp::make_subcommand(),
            timer::make_subcommand(),
            uart0::make_subcommand(),
            usart::make_subcommand(),
            usb::make_subcommand(),
            wdt::make_subcommand(),
        ])
}

pub fn execute(args: &ArgMatches) -> Result<()> {
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
        (unknown, _) => { println!("Unrecognized module: {}", unknown); Ok(()) },
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

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("adc")
            .about("Analog to Digital Converter")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod button {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("button")
            .about("Flipper's built-in Button")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod dac {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("dac")
            .about("Digital to Analog Converter")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod fs {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("fs")
            .about("Filesystem")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod gpio {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("gpio")
            .about("General-Purpose Input/Output")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod i2c {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("i2c")
            .about("Inter-IC (integrated circuit) bus")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
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

        let flipper = flipper::Flipper::attach();
        flipper::fsm::led::rgb(red, green, blue);
        Ok(())
    }
}

pub mod pwm {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("pwm")
            .about("Pulse-Width Modulation")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod rtc {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("rtc")
            .about("Real-Time Clock")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod spi {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("spi")
            .about("Serial Peripheral Interface")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod swd {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("swd")
            .about("Serial Wire Debug")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod temp {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("temp")
            .about("Temperature")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod timer {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("timer")
            .about("General purpose Timer")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod uart0 {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("uart0")
            .about("Universal Asynchronous Receive/Transmit bus")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod usart {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("usart")
            .about("Universal Synchronous/Asynchronous Receive/Transmit bus")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod usb {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("usb")
            .about("Universal Serial Bus")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}

pub mod wdt {
    pub use super::*;

    pub fn make_subcommand<'a, 'b>() -> App<'a, 'b> {
        App::new("wdt")
            .about("WatchDog Timer")
    }

    pub fn execute(args: &ArgMatches) -> Result<()> {
        unimplemented!();
    }
}
