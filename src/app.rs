use std::collections::HashMap;

use clap::{App, AppSettings, Arg, ArgGroup, SubCommand};

const ABOUT: &'static str = "flipper: Manage and control Flipper from the command line";

pub fn app() -> App<'static, 'static> {
    let arg = |name| {
        Arg::with_name(name)
            .help(USAGES[name].short)
            .long_help(USAGES[name].long)
    };
    let flag = |name| arg(name).long(name);

    let sub = |name| {
        SubCommand::with_name(name)
            .about(USAGES[name].short)
            .long_about(USAGES[name].long)
    };

    let lang_flags: &[Arg] = &[
        flag("java").short("j"),
        flag("javascript").short("js"),
        flag("python").short("py"),
        flag("objc").short("o"),
        flag("swift").short("s"),
        flag("rust").short("r"),
    ];

    App::new("flipper")
        .author(crate_authors!())
        .version(crate_version!())
        .about(ABOUT)
        .max_term_width(100)
        .settings(&[
            AppSettings::AllowExternalSubcommands,
            AppSettings::ArgRequiredElseHelp,
            AppSettings::ColoredHelp,
            AppSettings::DeriveDisplayOrder,
            AppSettings::UnifiedHelpMessage,
        ])
        .subcommands(vec![
            // Module subcommand
            module_app(),

            // Package management subcommands
            sub("init").args(lang_flags),
            sub("new")
                .about("Create a new Flipper project with the given name")
                .args(lang_flags)
                .arg(arg("project")
                    .required(true)
                    .takes_value(true)
                    .value_name("project")
                    .help("The name of the new project")),
            sub("add")
                .about("Add a new module dependency to this project")
                .usage("flipper add [--save] [--save-dev] <module>")
                .setting(AppSettings::ArgRequiredElseHelp)
                .args(&[
                    flag("save").short("-b")
                        .help("Add as a build dependency"),
                    arg("save-dev")
                        .short("-D")
                        .long("--save-dev")
                        .help("Add as a dev dependency"),
                    arg("module")
                        .required(true)
                        .takes_value(true)
                        .value_name("module")
                        .help("The name of a module in the Flipper Module Repository"),
                ]),
            sub("remove")
                .alias("rm")
                .about("Remove a module dependency from this project")
                .setting(AppSettings::ArgRequiredElseHelp)
                .arg(arg("module")
                    .required(true)
                    .takes_value(true)
                    .value_name("module")
                    .help("The module to remove as a dependency")
                ),
            sub("update")
                .about("Update module dependencies to the latest versions"),

            // Management subcommands
            sub("boot"),
            sub("install")
                .about("Install a Flipper package onto the device. \
                                      Installed packages persist on device reset.")
                .before_help("Install the current-project package, or [package] if given")
                .arg(arg("package")
                    .required(false)
                    .takes_value(true)
                    .value_name("package")
                    .help("Specifies a package to install, such as from the repository")
                ),
            sub("deploy")
                .about("Deploy a Flipper package onto the device. \
                                      Deployed packages are lost on device reset.")
                .before_help("Deploy the current-project package, or [package] if given")
                .arg(arg("package")
                    .required(false)
                    .takes_value(true)
                    .value_name("package")
                    .help("Specify a package to install, such as from the repository")
                ),
            sub("generate")
                .alias("gen")
                .about("Generate Flipper language bindings")
                .before_help("Generate bindings for the current-project module, or [module] if given")
                .args(lang_flags.clone())
                .arg(arg("module")
                    .takes_value(true)
                    .value_name("module")
                    .help("The name of the module to generate language bindings for")
                ),
        ])
}

pub fn module_app() -> App<'static, 'static> {
    let sub = |name| {
        SubCommand::with_name(name)
            .about(USAGES[name].short)
            .long_about(USAGES[name].long)
    };

    sub("module")
        .settings(&[AppSettings::AllowExternalSubcommands, AppSettings::ArgRequiredElseHelp])
        .alias("-m")
        .template(
            "{bin} \n\
             {before-help} \n\n\
             USAGE: \n    {usage} \n\n\
             {all-args}"
        )
        .before_help("Control modules from the command line or use the interactive REPL")
        .arg(Arg::with_name("repl")
            .short("-r")
            .long("--repl")
            .help("Enter a Read-Eval-Print-Loop for interactive module control")
        )
        .subcommands(vec![
            sub("adc"),
            sub("button"),
            sub("dac"),
            sub("fs"),
            sub("gpio"),
            sub("i2c"),
            sub("led"),
            sub("pwm"),
            sub("rtc"),
            sub("spi"),
            sub("swd"),
            sub("temp"),
            sub("timer"),
            sub("uart0"),
            sub("usart"),
            sub("usb"),
            sub("wdt"),
        ])
}

struct Usage {
    short: &'static str,
    long: &'static str,
}

macro_rules! doc {
    ($map:expr, $name:expr, $short:expr) => {
        doc!($map, $name, $short, $short)
    };
    ($map:expr, $name:expr, $short:expr, $long:expr) => {
        $map.insert($name, Usage {
            short: $short,
            long: concat!($long, "\n "),
        });
    };
}

lazy_static! {
    static ref USAGES: HashMap<&'static str, Usage> = {
        let mut h = HashMap::new();
        doc!(h, "init",
            "Create a new project in the current directory",
            "Creates a new Flipper project in the current directory. This includes a default \
             package file and .gitignore, as well as blank templates for any language bindings \
             that were specified.");
        doc!(h, "new",
            "Create a new project in a target directory",
            "Creates a new Flipper project in the target directory. This includes a default \
             package file and .gitignore, as well as blank templates for any language bindings \
             that were specified.");
        doc!(h, "add",
            "Add a new module dependency",
            "");
        doc!(h, "adc", "Control the analog-to-digital module");
        doc!(h, "button", "Control the button module");
        doc!(h, "dac", "Control the digital-to-analog module");
        doc!(h, "fs", "Control the filesystem module");
        doc!(h, "gpio", "Control the general-purpose input-output module");
        doc!(h, "i2c", "Control the i2c module");
        doc!(h, "led", "Control the RGB LED on Flipper");
        doc!(h, "pwm", "Control the pulse-width modulation module");
        doc!(h, "rtc", "Control the real-time clock");
        doc!(h, "spi", "Control the spi bus");
        doc!(h, "swd", "Control the swd module");
        doc!(h, "temp", "Control the temperature module");
        doc!(h, "timer", "Control the timer module");
        doc!(h, "uart0", "Control the universal asynchronous bus on port 0");
        doc!(h, "usart", "Control the universal synchronous/asynchronous bus");
        doc!(h, "usb", "Control the usb module");
        doc!(h, "wdt", "Control the wdt module");
        h
    };
}
