extern crate flipper;
#[macro_use]
extern crate clap;

use flipper::fsm;
use clap::{App, AppSettings};

fn main() {
    let config = load_yaml!("flipper.yaml");
    let matches = App::from_yaml(config)
        .setting(AppSettings::AllowExternalSubcommands)
        .setting(AppSettings::DeriveDisplayOrder)
        .get_matches();

    match matches.subcommand() {

        // Flipper standard module control
        (c @ "adc",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "button", Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "dac",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "fs",     Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "gpio",   Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "i2c",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "led",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "pwm",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "rtc",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "spi",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "swd",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "temp",   Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "timer",  Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "uart0",  Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "usart",  Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "usb",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),
        (c @ "wdt",    Some(sub_m)) => println!("Flipper-{}: {:?}", c, sub_m),

        // If this case is reached, then no built-in command matched.
        (external, Some(sub_m)) => {
            println!("External command {:?}, submatch: {:?}", external, sub_m);
        },

        // If this case is reached, no subcommands or args were provided (i.e. ./flipper)
        (_, None) => println!("Nonexistent command!"),
    };
}
