extern crate flipper;

use flipper::{Flipper, ModuleFFI, UserModule, UserModuleFFI};
use flipper::fmr::{
    Args,
    lf_invoke,
};

struct GpioToggle {
    ffi: ModuleFFI,
}

impl<'a> UserModule<'a> for GpioToggle {
    const NAME: &'a str = "gpio";
    fn new() -> Self {
        GpioToggle {
            ffi: ModuleFFI::User(UserModuleFFI::uninitialized(Self::NAME)),
        }
    }
}

impl From<UserModuleFFI> for GpioToggle {
    fn from(user: UserModuleFFI) -> Self {
        GpioToggle {
            ffi: ModuleFFI::User(user),
        }
    }
}

impl GpioToggle {
    fn toggle(&self) {
        let args = Args::new().append(4u8);
        lf_invoke(&self.ffi, 0, args)
    }
}

fn main() {
    let flipper = Flipper::attach();
    let gpio: GpioToggle = GpioToggle::bind(&flipper);
    gpio.toggle();
}