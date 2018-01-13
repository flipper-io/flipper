extern crate flipper;

use flipper::{Flipper, StandardModule};
use flipper::fsm::led::Led;

fn main() {
    Flipper::attach();
    let led = Led::new();
    led.rgb(10, 0, 0);
}
