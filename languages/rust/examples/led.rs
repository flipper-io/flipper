extern crate flipper;

use flipper::{Flipper, StandardModule};
use flipper::fsm::led::Led;

fn main() {
    let _flipper = Flipper::attach().expect("should attach to Flipper");
    let led = Led::new();
    led.rgb(10, 0, 0);
}
