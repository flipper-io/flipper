#![allow(non_upper_case_globals)]

use std::io::{
    Read,
    Write,
    Result,
};

use ::{
    Flipper,
    StandardModule,
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

pub struct Uart0 {
    ffi: ModuleFFI,
}

impl StandardModule for Uart0 {
    fn new() -> Self {
        unsafe {
            let ffi = StandardModuleFFI { module_meta: &_uart0 };
            Uart0 {
                ffi: ModuleFFI::Standard(ffi),
            }
        }
    }
    /// Instantiates a Uart0 module bound to a specific Flipper device.
    // TODO implement binding
    fn bind(_: &Flipper) -> Self {
        unsafe {
            let ffi = StandardModuleFFI { module_meta: &_uart0 };
            Uart0 {
                ffi: ModuleFFI::Standard(ffi),
            }
        }
    }
}

impl Uart0 {
    /// Configures the Uart0 module with a given baud rate and
    /// interrupts enabled flag.
    pub fn configure(&self, baud: UartBaud, interrupts: bool) {
        let args = Args::new()
            .append(baud.to_baud())
            .append(if interrupts { 1u8 } else { 0u8 });
        lf_invoke(lf_get_current_device(), &self.ffi, 0, args)
    }

    /// Indicates whether the Uart0 bus is ready to read or write.
    pub fn ready(&self) -> bool {
        let ret: u8 = lf_invoke(lf_get_current_device(), &self.ffi, 1, Args::new());
        ret != 0
    }
}

impl Write for Uart0 {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        lf_push::<()>(&self.ffi, 2, buf, Args::new());
        Ok(buf.len())
    }
    fn flush(&mut self) -> Result<()> {
        Ok(())
    }
}

impl Read for Uart0 {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        if buf.len() == 0 { return Ok(0) }
        lf_pull::<()>(&self.ffi, 3, buf, Args::new());
        Ok(buf.len())
    }
}
