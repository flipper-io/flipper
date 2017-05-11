extern crate clap;

mod arg_builder;

fn main() {
    let matches = arg_builder::build::<'static, 'static>().get_matches();
}
