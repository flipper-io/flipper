extern crate flipper;
use flipper::led;

fn main() {
    let device = flipper::attach();
    led::configure();
    led::rgb(0, 0, 0);
    println!("Attached to device {:?}.", device.name());
}
