extern crate clap;

mod app_builder;

fn main() {
    let app = app_builder::build::<'static>();
    let matches = app.get_matches();

    match matches.subcommand() {
        ("pwm", Some(sub_m)) => println!("Command pwm!"),
        ("one", Some(sub_m)) => println!("Command one, submatch: {:?}", sub_m),
        ("two", Some(sub_m)) => println!("Command two, submatch: {:?}", sub_m),
        (external, Some(sub_m)) => println!("External command {:?}, submatch: {:?}", external, sub_m),
        (_, None) => println!("Nonexistent command!"),
    }
}
