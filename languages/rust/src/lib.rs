#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

extern crate libc;

pub mod fsm;

use std::ffi::{CString, CStr};
use libc::{c_void, c_char, c_int};

pub const LF_VERSION: u16 = 0x0001;

type _fmr_arg_index = u8;
type _fmr_return = u32;

/// The libflipper native representation of a linked list. We need this
/// representation so we can construct parameter lists for FMR invocations.
#[repr(C)]
#[derive(Debug)]
struct _lf_ll {
    item: *const c_void,
    destructor: *const c_void,
    next: *const _lf_ll,
}

/// The libflipper native representation of a module. An `_lf_module` struct
/// is the most important piece of information in FMR because it's name is a
/// unique identifier, its index maps it to a specific memory location on the
/// device, and the device pointer specifies which Flipper this module sends
/// calls to.
#[repr(C)]
#[derive(Debug)]
struct _lf_module {
    /// The name and unique identifier of this module.
    name: *const c_char,
    /// An optional description of what this module does.
    description: *const c_char,
    /// A version of this module. Important to update when APIs change so that
    /// FMR can keep track of how to execute functions correctly.
    version: u16,
    /// A checksum of the module.
    crc: u16,
    /// The index into Flipper's module store where the native code for this
    /// module is kept. Flipper loads modules sequentially, with the standard
    /// modules automatically pre-loaded from 0-X.
    index: u16,
    /// A pointer representing the specific Flipper device on which this module's
    /// native code lives and therefore where calls using this module will be
    /// executed. If `NULL`, calls will automatically go to the currently-selected
    /// Flipper, as tracked by libflipper.
    device: *const c_void,
}

/// The public facing struct representing a Flipper Module keeps the details
/// of module representation a secret in order to prevent breaking API changes.
#[derive(Debug)]
pub struct Module {
    module: _lf_module,
}

#[link(name = "flipper")]
extern {
    fn flipper_attach() -> *const c_void;
    fn carbon_attach_hostname(hostname: *const c_char) -> *const c_void;
    fn flipper_detach(device: *const c_void) -> i32;
    fn lf_invoke(module: *const _lf_module, function: _fmr_arg_index, ret: u8, args: *const _lf_ll) -> _fmr_return;
    fn lf_ll_append(ll: *mut *mut _lf_ll, item: *const c_void, destructor: *const c_void) -> c_int;
}

pub struct Flipper {
    /* Reference to the _lf_device struct. */
    _device: *const c_void,
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
}

impl Drop for Flipper {
    fn drop(&mut self) {
        unsafe {
            flipper_detach(self._device)
        };
    }
}
