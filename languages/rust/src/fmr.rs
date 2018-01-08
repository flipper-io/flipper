//! Provides utilities for defining and executing remote calls using FMR.
//!
//! The Flipper Message Runtime (FMR) is the mechanism that Flipper uses to
//! remotely execute functions on the Flipper device from a Host machine such
//! as a desktop computer or phone.
//!
//! These utilities lay the groundwork for users to create bindings to custom
//! Flipper "modules" (not to be confused with rust modules).

#![allow(non_camel_case_types)]

use libc;
use libc::{c_void, c_int, size_t};
use std::ops::Deref;
use std::mem;
use std::ptr;
use ::{ModuleFFI, _lf_module};

/// Types transmitted over FMR are encoded in a u8.
type _lf_type = libc::uint8_t;

/// Values transmitted over FMR are packaged in a u64.
type _lf_value = libc::uint64_t;

/// Function indices are represented by a u8.
type _lf_index = libc::uint8_t;

// The concrete encodings for types in libflipper.
const LF_TYPE_U8: _lf_type = 0;
const LF_TYPE_U16: _lf_type = 1;
const LF_TYPE_VOID: _lf_type = 2;
const LF_TYPE_U32: _lf_type = 3;
const LF_TYPE_PTR: _lf_type = 4;
const LF_TYPE_INT: _lf_type = 6;
const LF_TYPE_U64: _lf_type = 7;

/// The internal `libflipper` representation of a function argument.
/// This is used for FFI when we ask libflipper to execute a function
/// on a device.
#[repr(C)]
#[derive(Debug, PartialEq, PartialOrd)]
struct _lf_arg {
    arg_type: _lf_type,
    arg_value: _lf_value,
}

/// The libflipper native representation of a linked list. We need this
/// representation so we can construct parameter lists for FMR invocations.
#[repr(C)]
pub(crate) struct _lf_ll {
    item: *const _lf_arg,
    destructor: *const c_void,
    next: *const _lf_ll,
}

#[link(name = "flipper")]
extern {
    fn lf_ll_append(ll: *mut *mut _lf_ll, item: *const c_void, destructor: *const c_void) -> c_int;
    fn lf_invoke(module: *const _lf_module, function: _lf_index, ret: u8, args: *const _lf_ll) -> _lf_value;
    fn lf_push(module: *const _lf_module, function: _lf_index, source: *const c_void, length: u32, args: *const _lf_ll) -> _lf_value;
    fn lf_pull(module: *const _lf_module, function: _lf_index, dest: *mut c_void, length: u32, args: *const _lf_ll) -> _lf_value;
}

/// Represents valid argument types for FMR. Currently, this only
/// includes `u8`, `u16`, and `u32`.
pub struct FmrArg(_lf_arg);

impl From<u8> for FmrArg {
    fn from(value: u8) -> FmrArg {
        FmrArg(_lf_arg {
            arg_type: LF_TYPE_U8,
            arg_value: value as _lf_value,
        })
    }
}

impl From<u16> for FmrArg {
    fn from(value: u16) -> FmrArg {
        FmrArg(_lf_arg {
            arg_type: LF_TYPE_U16,
            arg_value: value as _lf_value,
        })
    }
}

impl From<u32> for FmrArg {
    fn from(value: u32) -> FmrArg {
        FmrArg(_lf_arg {
            arg_type: LF_TYPE_U32,
            arg_value: value as _lf_value,
        })
    }
}

/// Describes the return types we can expect from an FMR call.
///
/// An FmrReturn is used when constructing an `FmrInvocation`
/// and allows the message system to know what return type it
/// will be carrying back from a successful invocation.
pub enum FmrReturn {
    U8,
    U16,
    U32,
    Unit,
}

impl FmrReturn {
    fn fmr_type(&self) -> _lf_type {
        match *self {
            FmrReturn::U8  => LF_TYPE_U8,
            FmrReturn::U16 => LF_TYPE_U16,
            FmrReturn::U32 => LF_TYPE_U32,
            FmrReturn::Unit => LF_TYPE_VOID,
        }
    }
}

/// A struct for building and executing an FMR call.
///
/// Suppose there was a module loaded on Flipper with the following
/// function:
///
/// ```c
/// uint16_t foo(uint8_t a, uint16_t b, uint32_t c) { ... }
/// ```
///
/// To invoke that function on the device, one would write:
///
/// ```
/// use flipper::{ModuleFFI};
/// use flipper::fmr::{FmrInvocation, FmrReturn};
///
/// struct MyModule {
///     ffi: ModuleFFI,
/// }
///
/// impl MyModule {
///     /// Here we make a function whose implementation is to create an
///     /// FmrInvocation from its arguments and cast the proper return type.
///     fn foo(&self, one: u8, two: u16, three: u32) -> u16 {
///         const INDEX: u8 = 0;
///         FmrInvocation::new(&self.ffi, "foo", INDEX, FmrReturn::U16)
///             .append(one)
///             .append(two)
///             .append(three)
///             .invoke() as u16
///     }
/// }
/// ```
pub struct FmrInvocation<'a> {
    module: &'a ModuleFFI,
    name: &'a str,
    index: u8,
    args: Vec<FmrArg>,
    return_type: FmrReturn,
}

impl<'a> Deref for FmrInvocation<'a> {
    type Target = Vec<FmrArg>;
    fn deref(&self) -> &Self::Target {
        &self.args
    }
}

impl<'a> FmrInvocation<'a> {
    /// Creates a new FMR invocation.
    pub fn new(module: &'a ModuleFFI, name: &'a str, index: u8, return_type: FmrReturn) -> FmrInvocation<'a> {
        FmrInvocation { module: module.into(), name, args: Vec::new(), index, return_type }
    }

    /// Appends an arg to this invocation. This allows invocations to be
    /// built using a builder pattern.
    pub fn append<T: Into<FmrArg>>(mut self, arg: T) -> Self {
        self.args.push(arg.into());
        self
    }

    /// Performs the FMR call described by this `FmrInvocation`.
    pub fn invoke(&self) -> u64 {
        unsafe {
            let mut arglist: *mut _lf_ll = ptr::null_mut();
            for arg in self.args.iter() {
                lf_ll_append(&mut arglist, &arg.0 as *const _lf_arg as *const c_void, ptr::null());
            }
            lf_invoke(self.module.as_ptr(), self.index, self.return_type.fmr_type(), arglist)
        }
    }

    /// Performs this FMR call as an `lf_push`, passing the data from the
    /// data slice to the device during the call.
    pub fn invoke_push(&self, data: &[u8]) -> u64 {
        unsafe {
            let mut arglist: *mut _lf_ll = ptr::null_mut();
            for arg in self.args.iter() {
                lf_ll_append(&mut arglist, &arg.0 as *const _lf_arg as *const c_void, ptr::null());
            }
            lf_push(self.module.as_ptr(), self.index, data.as_ptr() as *const c_void, data.len() as u32, arglist)
        }
    }

    /// Performs this FMR call as an `lf_pull`, bringing data from the
    /// device and loading it into the buffer slice. This operation
    /// pulls exactly as much data as the size of the buffer slice.
    pub fn invoke_pull(&self, buffer: &mut [u8]) -> u64 {
        unsafe {
            let mut arglist: *mut _lf_ll = ptr::null_mut();
            for arg in self.args.iter() {
                lf_ll_append(&mut arglist, &arg.0 as *const _lf_arg as *const c_void, ptr::null());
            }
            lf_pull(self.module.as_ptr(), self.index, buffer.as_mut_ptr() as *mut c_void, buffer.len() as u32, arglist)
        }
    }
}

mod test {
    use super::*;
    use ::UserModuleFFI;

    #[test]
    fn test_fmr_arg() {
        let argu8 = FmrArg::from(123u8);
        let argu8_native = argu8.0;
        assert_eq!(argu8_native.arg_type, LF_TYPE_U8);
        assert_eq!(argu8_native.arg_value, 123u64);

        let argu16 = FmrArg::from(234u16);
        let argu16_native = argu16.0;
        assert_eq!(argu16_native.arg_type, LF_TYPE_U16);
        assert_eq!(argu16_native.arg_value, 234u64);

        let argu32 = FmrArg::from(345u32);
        let argu32_native = argu32.0;
        assert_eq!(argu32_native.arg_type, LF_TYPE_U32);
        assert_eq!(argu32_native.arg_value, 345u64);
    }

    #[test]
    fn test_fmr_builder() {
        let module = UserModuleFFI::from(("testMod", 0, 0, 0));
        let ffi = ModuleFFI::User(module);
        let function = FmrInvocation::new(&ffi, "test", 0, FmrReturn::Unit)
            .append(1u8)
            .append(2u16)
            .append(3u32)
            .append(4u8)
            .append(5u16);

        let expected = vec![
            _lf_arg { arg_type: LF_TYPE_U8, arg_value: 1u64 },
            _lf_arg { arg_type: LF_TYPE_U16, arg_value: 2u64 },
            _lf_arg { arg_type: LF_TYPE_U32, arg_value: 3u64 },
            _lf_arg { arg_type: LF_TYPE_U8, arg_value: 4u64 },
            _lf_arg { arg_type: LF_TYPE_U16, arg_value: 5u64 },
        ];

        for (actual, expected) in function.iter().zip(expected) {
            assert_eq!(actual.0, expected);
        }
    }
}