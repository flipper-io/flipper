#![allow(non_upper_case_globals)]

use ::{
    Flipper,
    StandardModule,
    StandardModuleFFI,
    ModuleFFI,
    _lf_module,
};

use fmr::{
    Args,
    lf_invoke,
};

#[link(name = "flipper")]
extern {
    static _gpio: _lf_module;
}

pub struct Gpio {
    module: ModuleFFI,
}

impl StandardModule for Gpio {
    fn new() -> Self {
        unsafe {
            let module = StandardModuleFFI { module_meta: &_gpio };
            Gpio {
                module: ModuleFFI::Standard(module),
            }
        }
    }

    fn bind(flipper: &Flipper) -> Self {
        unsafe {
            let module = StandardModuleFFI { module_meta: &_gpio };
            Gpio {
                module: ModuleFFI::Standard(module),
            }
        }
    }
}

impl Gpio {
    pub fn configure(&self) {
        lf_invoke(&self.module, 0, Args::new())
    }

    pub fn enable(&self, enable: u32, disable: u32) {
        let args = Args::new()
            .append(enable)
            .append(disable);
        lf_invoke(&self.module, 1, args)
    }

    pub fn write(&self, set: u32, clear: u32) {
        let args = Args::new()
            .append(set)
            .append(clear);
        lf_invoke(&self.module, 2, args)
    }

    pub fn read(&self, mask: u32) -> u32 {
        let args = Args::new()
            .append(mask);
        lf_invoke(&self.module, 3, args)
    }
}
