extern crate flipper;

use flipper::{
    lf,
    Flipper,
};

struct GpioToggle<'a> {
    flipper: &'a Flipper,
}

impl<'a> GpioToggle<'a> {
    pub fn new(flipper: &'a Flipper) -> GpioToggle<'a> {
        GpioToggle { flipper }
    }

    pub fn toggle(&self) {
        let args = lf::Args::new()
            .append(4u8);
        lf::invoke(self.flipper, "gpio", 0, args)
    }
}

fn main() {
    let flipper = Flipper::attach().expect("should attach to Flipper");
    let gpio: GpioToggle = GpioToggle::new(&flipper);
    gpio.toggle();
}