#[macro_use]
extern crate clap;
extern crate rustyline;

mod app;
mod management;
mod modules;
mod package_manager;

fn main() {
    app::execute(&app::app().get_matches());
}
