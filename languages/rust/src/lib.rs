#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate libc;

pub mod fsm;

use std::ffi::{CString, CStr};
use std::str;
use std::fmt;
use libc::{c_char, c_void};

#[repr(C)]
struct _lf_configuration {
    name: [c_char; 16],
    identifier: i32,
    version: i32,
    attributes: i8
}

#[repr(C)]
struct _lf_device {
    configuration: _lf_configuration,
    endpoint: *const c_void,
    error: i32
}

#[link(name = "flipper")]
extern {
    fn flipper_attach() -> *mut _lf_device;
    fn carbon_attach_hostname(hostname: *const c_char) -> *mut _lf_device;
    fn flipper_select(device: *mut _lf_device) -> i32;
    fn flipper_detach(device: *mut _lf_device) -> i32;
}

pub struct Flipper {
    /* Reference to the _lf_device struct. */
    _device: *mut _lf_device,
}

impl Flipper {

    pub fn attach() -> Self {
        unsafe {
            Flipper {
                _device: flipper_attach()
            }
        }
    }

    pub fn attach_hostname(hostname: &str) -> Self {
        unsafe {
            Flipper {
                _device: carbon_attach_hostname(CString::new(hostname).unwrap().as_ptr())
            }
        }
    }

    pub fn select(&self) {
        unsafe {
            flipper_select(self._device)
        };
    }

    /// Returns the name of the device as a string slice.
    pub fn name(&self) -> &str {
        let c_buf: *const c_char = unsafe { (*self._device).configuration.name.as_ptr() };
        let c_str: &CStr = unsafe { CStr::from_ptr(c_buf) };
        let buf: &[u8] = c_str.to_bytes();
        return str::from_utf8(buf).unwrap();
    }

    pub fn identifier(&self) -> i32 {
        unsafe {
            (*self._device).configuration.identifier
        }
    }

    pub fn version(&self) -> i32 {
        unsafe {
            (*self._device).configuration.version
        }
    }

    pub fn attributes(&self) -> i8 {
        unsafe {
            (*self._device).configuration.attributes
        }
    }
}

impl fmt::Display for Flipper {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "name: {}\nidentifier: {:#x}\nversion: {:#x}\nattributes: {:#x}", self.name(), self.identifier(), self.version(), self.attributes())
    }
}

impl Drop for Flipper {
    fn drop(&mut self) {
        unsafe {
            flipper_detach(self._device)
        };
    }
}

mod test {

    #[test]
    fn test() {
        /* Attach to a device. */
        let device = attach();
        /* Print the device info. */
        println!("{}", device);
        /* Configure the LED peripheral. */
        fsm::led::configure();
        /* Set the LED color. */
        fsm::led::rgb(0, 10, 0);
        /* Do some gpio stuff. */
        fsm::gpio::enable((1 << 0), 0);
        fsm::gpio::write((1 << 0), 0);
    }
}
