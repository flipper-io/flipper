extern crate flipper;

use flipper::{Flipper, ModuleFFI, UserModule, UserModuleFFI};
use flipper::fmr::{
    Args,
    lf_invoke,
};

struct GpioToggle<'a> {
    flipper: &'a Flipper,
    module: ModuleFFI,
}

impl<'a> UserModule<'a> for GpioToggle<'a> {
    const NAME: &'a str = "gpio";
    fn new(flipper: &'a Flipper) -> Self {
        GpioToggle {
            flipper,
            module: ModuleFFI::User(UserModuleFFI::uninitialized(Self::NAME)),
        }
    }
}

impl<'a> From<(&'a Flipper, UserModuleFFI)> for GpioToggle<'a> {
    fn from((flipper, module): (&'a Flipper, UserModuleFFI)) -> Self {
        GpioToggle {
            flipper,
            module: ModuleFFI::User(module),
        }
    }
}

impl<'a> GpioToggle<'a> {
    fn toggle(&self) {
        let args = Args::new().append(4u8);
        lf_invoke(self.flipper, &self.module, 0, args)
    }
}

fn main() {
    let flipper = Flipper::attach().expect("should attach to Flipper");
    let gpio: GpioToggle = GpioToggle::new(&flipper);
    gpio.toggle();
}