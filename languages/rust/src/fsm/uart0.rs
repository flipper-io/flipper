#![allow(non_upper_case_globals)]

use std::io::{Read, Write, Result};
use std::ffi::CString;
use std::ptr;
use libc::c_void;

use ::{
    Flipper,
    DEFAULT_FLIPPER,
    Module,
    ModuleFFI,
    StandardModuleFFI,
    _lf_module,
    _lf_device
};

use fmr::{FmrInvocation, FmrReturn};

#[link(name = "flipper")]
extern {
    static mut _uart0: _lf_module;
}

pub enum UartBaud {
    FMR,
    DFU,
}

impl UartBaud {
    fn to_baud(&self) -> u8 {
        match *self {
            UartBaud::FMR => 0x00,
            UartBaud::DFU => 0x10,
        }
    }
}

pub struct Uart0 {
    ffi: ModuleFFI,
}

impl Module for Uart0 {
    fn name<'a>() -> &'a str { "uart0" }
    fn new() -> Self {
        unsafe {
            let ffi = StandardModuleFFI { ffi: &mut _uart0 as *mut _lf_module };
            Uart0 {
                ffi: ModuleFFI::Standard(ffi),
            }
        }
    }
}

impl Uart0 {
    /// Instantiates a Uart0 module bound to a specific Flipper device.
    pub fn bind(flipper: &Flipper) -> Self {
        let device = flipper.device;
        unsafe {
            let ffi = StandardModuleFFI { ffi: &mut _uart0 as *mut _lf_module };
            Uart0 {
                ffi: ModuleFFI::Standard(ffi),
            }
        }
    }

    /// Configures the Uart0 module with a given baud rate and
    /// interrupts enabled flag.
    pub fn configure(&self, baud: &UartBaud, interrupts: bool) {
        FmrInvocation::new(&self.ffi, "configure", 0, FmrReturn::Unit)
            .append(baud.to_baud())
            .append(if interrupts { 1u8 } else { 0u8 })
            .invoke();
    }

    /// Indicates whether the Uart0 bus is ready to read or write.
    pub fn ready(&self) -> bool {
        FmrInvocation::new(&self.ffi, "ready", 1, FmrReturn::U8)
            .invoke() as u8 != 0u8
    }
}

impl Write for Uart0 {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        FmrInvocation::new(&self.ffi, "write", 2, FmrReturn::Unit).invoke_push(buf);
        Ok(buf.len())
    }

    fn flush(&mut self) -> Result<()> { Ok(()) }
}

impl Read for Uart0 {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        FmrInvocation::new(&self.ffi, "read", 3, FmrReturn::Unit).invoke_pull(buf);
        Ok(buf.len())
    }
}
