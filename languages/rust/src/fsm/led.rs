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
    static _led: _lf_module;
}

pub struct Led {
    module: ModuleFFI,
}

impl StandardModule for Led {
    fn new() -> Self {
        unsafe {
            let module = StandardModuleFFI { module_meta: &_led };
            Led {
                module: ModuleFFI::Standard(module),
            }
        }
    }

    fn bind(_: &Flipper) -> Self {
        unsafe {
            let module = StandardModuleFFI { module_meta: &_led };
            Led {
                module: ModuleFFI::Standard(module),
            }
        }
    }
}

impl Led {
    pub fn configure(&self) {
        lf_invoke(lf_get_current_device(), &self.module, 0, Args::new())
    }

    pub fn rgb(&self, r: u8, g: u8, b: u8) {
        let args = Args::new()
            .append(r)
            .append(g)
            .append(b);
        lf_invoke(lf_get_current_device(), &self.module, 1, args)
    }
}
