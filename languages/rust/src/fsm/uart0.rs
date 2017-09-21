use std::io::{Read, Write, Result};
use libc::c_void;

use ::Flipper;

#[link(name = "flipper")]
extern {
    fn uart0_configure() -> u32;
    fn uart0_enable();
    fn uart0_disable();
    fn uart0_dfu();
    fn uart0_ready() -> u8;
    fn uart0_push(source: *const c_void, length: usize);
    fn uart0_pull(dest: *mut c_void, length: usize, timeout: u32);
}

pub struct Uart0<'a>(&'a Flipper);

impl <'a> Uart0<'a> {
    pub fn new(flipper: &'a Flipper) -> Self {
        Uart0(flipper)
    }
}

impl <'a> ::std::ops::Deref for Uart0<'a> {
    type Target = &'a Flipper;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl <'a> Read for Uart0<'a> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        unsafe {
            uart0_pull(buf.as_mut_ptr() as *mut c_void, buf.len(), 0xff)
        };
        Ok(buf.len())
    }
}

impl <'a> Write for Uart0<'a> {
    fn write(&mut self, buf: &[u8]) -> Result<usize> {
        self.0.select();
        unsafe {
            uart0_push(buf.as_ptr() as *const c_void, buf.len())
        }
        Ok(buf.len())
    }

    fn flush(&mut self) -> Result<()> {
        unimplemented!()
    }
}

pub fn configure() {
    unsafe { uart0_configure() };
}

pub fn ready() -> bool {
    unsafe { uart0_ready() != 0 }
}

pub fn enable() {
    unsafe { uart0_enable() }
}

pub fn disable() {
    unsafe { uart0_disable() }
}

pub fn dfu() {
    unsafe { uart0_dfu() };
}
