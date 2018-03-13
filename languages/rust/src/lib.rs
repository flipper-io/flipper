#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![deny(unstable_features)]
#![deny(unused_import_braces)]
#![deny(unused_qualifications)]
//#![deny(warnings)]

#[allow(unused_imports)]
#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;
extern crate libc;

pub mod fsm;
pub mod lf;

use libc::{c_void, c_char};
use std::ffi::CString;
use std::ptr;

type _lf_device = *const c_void;
type _lf_function_index = u8;
type _fmr_return = u32;

pub const LF_VERSION: u16 = 0x0001;

#[derive(Debug, Fail)]
pub enum FlipperError {
    #[fail(display = "failed to attach to a Flipper device")]
    Attach,
}

type Result<T> = std::result::Result<T, FlipperError>;

#[link(name = "flipper")]
extern {
    fn flipper_attach() -> _lf_device;
    fn carbon_attach_hostname(hostname: *const c_char) -> _lf_device;
}

pub struct Flipper {
    /// A reference to an active Flipper profile in libflipper. This
    /// is used when communicating with libflipper to specify which
    /// device functions should be executed on.
    device: _lf_device
}

impl Flipper {
    pub fn attach() -> Result<Flipper> {
        unsafe {
            let device = flipper_attach();
            if device == ptr::null() { return Err(FlipperError::Attach); }
            Ok(Flipper { device })
        }
    }

    pub fn attach_hostname(hostname: &str) -> Result<Flipper> {
        unsafe {
            let hostname_string = CString::new(hostname).unwrap();
            let device = carbon_attach_hostname(hostname_string.as_ptr());
            if device == ptr::null() { return Err(FlipperError::Attach); }
            Ok(Flipper { device })
        }
    }
}
