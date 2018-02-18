#![allow(non_upper_case_globals)]

use std::io::{
    Read,
    Write,
    Result,
};

use ::{
    Flipper,
    ModuleFFI,
    StandardModuleFFI,
    _lf_module,
};

use fmr::{
    Args,
    lf_invoke,
    lf_push,
    lf_pull,
};

#[link(name = "flipper")]
extern {
    static _uart0: _lf_module;
}

pub enum UartBaud {
    FMR,
    DFU,
}

impl UartBaud {
    fn to_baud(&self) -> u8 {
        match *self {
            UartBaud::FMR => 0x00,
            UartBaud::DFU => 0x08,
        }
    }
}

pub struct Uart0<'a> {
    flipper: &'a Flipper,
    module: ModuleFFI,
}

impl<'a> Uart0<'a> {
    pub fn new(flipper: &'a Flipper) -> Self {
        let module = unsafe { StandardModuleFFI { module_meta: &_uart0 } };
        let module = ModuleFFI::Standard(module);
        Uart0 { flipper, module }
    }

    /// Configures the Uart0 module with a given baud rate and
    /// interrupts enabled flag.
    pub fn configure(&self, baud: UartBaud, interrupts: bool) {
        let args = Args::new()
            .append(baud.to_baud())
            .append(if interrupts { 1u8 } else { 0u8 });
        lf_invoke(self.flipper, &self.module, 0, args)
    }

    /// Indicates whether the Uart0 bus is ready to read or write.
    pub fn ready(&self) -> bool {
        let ret: u8 = lf_invoke(self.flipper, &self.module, 1, Args::new());
        ret != 0
    }
}

impl<'a> Write for Uart0<'a> {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        lf_push::<()>(self.flipper, &self.module, 2, buf, Args::new());
        Ok(buf.len())
    }
    fn flush(&mut self) -> Result<()> {
        Ok(())
    }
}

impl<'a> Read for Uart0<'a> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        if buf.len() == 0 { return Ok(0) }
        lf_pull::<()>(self.flipper, &self.module, 3, buf, Args::new());
        Ok(buf.len())
    }
}
