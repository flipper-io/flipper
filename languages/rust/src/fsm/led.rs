#![allow(non_upper_case_globals)]

use ::{
    Flipper,
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
    static _led: _lf_module;
}

pub struct Led<'a> {
    flipper: &'a Flipper,
    module: ModuleFFI,
}

impl<'a> Led<'a> {
    pub fn new(flipper: &'a Flipper) -> Self {
        let module = unsafe { StandardModuleFFI { module_meta: &_led } };
        let module = ModuleFFI::Standard(module);
        Led { flipper, module }
    }

    pub fn configure(&self) {
        lf_invoke(self.flipper, &self.module, 0, Args::new())
    }

    pub fn rgb(&self, r: u8, g: u8, b: u8) {
        let args = Args::new()
            .append(r)
            .append(g)
            .append(b);
        lf_invoke(self.flipper, &self.module, 1, args)
    }
}
