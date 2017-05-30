use clap::{App, Arg, ArgMatches, SubCommand, AppSettings};

trait AppExt<'a, 'b> {
    /// Constructs the modules subcommand.
    fn subcommand_modules(self) -> Self;
}

impl <'a, 'b> AppExt<'a, 'b> for App<'a, 'b> {
    fn subcommand_modules(self) -> App<'a, 'b> {
        self.subcommand(
            SubCommand::with_name("adc")
                .arg(
                    Arg::with_name("value")
                        .help("Controls the adc module")
                )
        ).subcommand(
            SubCommand::with_name("button")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("dac")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("fs")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("gpio")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("i2c")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("led")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("pwm")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("rtc")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("spi")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("swd")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("temp")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("timer")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("uart0")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("usart")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("usb")
                .arg(Arg::with_name("value"))
        ).subcommand(
            SubCommand::with_name("wdt")
                .arg(Arg::with_name("value"))
        )
    }
}

pub fn build<'a>() -> App<'a, 'a> {
    App::new("Flipper Console")
        .version("0.0.1")
        .author("Nick Mosher <nicholastmosher@gmail.com>")
        .about("Command-line utility for managing and controlling Flipper")
        .setting(AppSettings::AllowExternalSubcommands)
        .subcommand_modules()
}
