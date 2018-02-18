extern crate flipper;

use flipper::Flipper;
use flipper::fsm::led::Led;

fn main() {
    let flipper = Flipper::attach();
    let led = Led::new(&flipper);
    led.rgb(10, 0, 0);
}
