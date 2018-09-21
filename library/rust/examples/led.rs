extern crate flipper;

use flipper::{
    Flipper,
    api::led::Led,
};

fn main() {
    let flipper = Flipper::attach().expect("should attach to Flipper");
    let led = Led::new(&flipper);
    led.rgb(10, 0, 0);
}
