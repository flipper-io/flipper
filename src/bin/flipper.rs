extern crate flipper;
extern crate clap;

use flipper::CLI;
use flipper::app;

fn main() {

    let flipper_app = app::app();
    let module_subcommand = app::module_app();

    let module_cli = flipper::modules::CLI::new(module_subcommand);

    match flipper_app.get_matches().subcommand() {
        ("module", Some(sub_match)) => module_cli.eval(sub_match),
        _ => unimplemented!(),
    }
}
