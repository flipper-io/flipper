//! Provides utilities for defining and executing remote calls using FMR.
//!
//! The Flipper Message Runtime (FMR) is the mechanism that Flipper uses to
//! remotely execute functions on the Flipper device from a Host machine such
//! as a desktop computer or phone.
//!
//! These utilities lay the groundwork for users to create bindings to custom
//! Flipper "modules" (not to be confused with rust modules).

use libc;
use libc::{c_void, c_int};
use std::ops::Deref;
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
const _LF_TYPE_PTR: _lf_type = 4;
const _LF_TYPE_INT: _lf_type = 6;
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

mod libflipper {
    use super::*;
    #[link(name = "flipper")]
    extern {
        pub(crate) fn lf_ll_append(ll: *mut *mut _lf_ll, item: *const c_void, destructor: *const c_void) -> c_int;
        pub(crate) fn lf_invoke(lf_get_current_device(), module: *const _lf_module, function: _lf_index, ret: u8, args: *const _lf_ll) -> _lf_value;
        pub(crate) fn lf_push(lf_get_current_device(), module: *const _lf_module, function: _lf_index, source: *const c_void, length: u32, args: *const _lf_ll) -> _lf_value;
        pub(crate) fn lf_pull(lf_get_current_device(), module: *const _lf_module, function: _lf_index, dest: *mut c_void, length: u32, args: *const _lf_ll) -> _lf_value;
    }
}

/// Represents an argument to an FMR call.
///
/// Any type which implement `Into<Arg>` can be appended to an `Args` list.
/// This currently includes `u8`, `u16`, `u32`, and `u64`.
///
/// ```
/// use flipper::fmr::Arg;
///
/// let one =   Arg::from(10 as u8);
/// let two =   Arg::from(20 as u16);
/// let three = Arg::from(30 as u32);
/// let four =  Arg::from(40 as u64);
/// ```
pub struct Arg(_lf_arg);

impl From<u8> for Arg {
    fn from(value: u8) -> Arg {
        Arg(_lf_arg {
            arg_type: LF_TYPE_U8,
            arg_value: value as _lf_value,
        })
    }
}

impl From<u16> for Arg {
    fn from(value: u16) -> Arg {
        Arg(_lf_arg {
            arg_type: LF_TYPE_U16,
            arg_value: value as _lf_value,
        })
    }
}

impl From<u32> for Arg {
    fn from(value: u32) -> Arg {
        Arg(_lf_arg {
            arg_type: LF_TYPE_U32,
            arg_value: value as _lf_value,
        })
    }
}

impl From<u64> for Arg {
    fn from(value: u64) -> Arg {
        Arg(_lf_arg {
            arg_type: LF_TYPE_U64,
            arg_value: value as _lf_value,
        })
    }
}

/// Represents an ordered, typed set of arguments to an FMR call. This is
/// to be used for calling `lf_invoke`.
///
/// ```
/// use flipper::fmr::Args;
///
/// let args = Args::new()
///                .append(10 as u8)
///                .append(20 as u16)
///                .append(30 as u32)
///                .append(40 as u64);
/// ```
pub struct Args(Vec<Arg>);

impl Args {
    pub fn new() -> Self {
        Args(Vec::new())
    }
    pub fn append<T: Into<Arg>>(mut self, arg: T) -> Self {
        self.0.push(arg.into());
        self
    }
}

impl Deref for Args {
    type Target = Vec<Arg>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// A container type for a value returned by performing an
/// `lf_invoke` call. Types which implement `LfReturnable`
/// must define how to extract their own representation
/// from this container.
pub struct LfReturn(u64);

/// A trait to be implemented for types which can be returned
/// from an `lf_invoke` call. Currently, only types up to
/// 64 bits can be represented. Any type which implements
/// `LfReturnable` must be able to extract itself from the
/// 64 bit representation in `LfReturn`.
///
/// Current `LfReturnable` types are `()`, `u8,` `u16`, `u32`,
/// and `u64`.
pub trait LfReturnable: From<LfReturn> {
    fn lf_type() -> _lf_type;
}

impl LfReturnable for () {
    fn lf_type() -> _lf_type { LF_TYPE_VOID }
}

impl From<LfReturn> for () {
    fn from(_: LfReturn) -> Self { () }
}

impl LfReturnable for u8 {
    fn lf_type() -> _lf_type { LF_TYPE_U8 }
}

impl From<LfReturn> for u8 {
    fn from(ret: LfReturn) -> Self {
        ret.0 as u8
    }
}

impl LfReturnable for u16 {
    fn lf_type() -> _lf_type { LF_TYPE_U16 }
}

impl From<LfReturn> for u16 {
    fn from(ret: LfReturn) -> Self {
        ret.0 as u16
    }
}

impl LfReturnable for u32 {
    fn lf_type() -> _lf_type { LF_TYPE_U32 }
}

impl From<LfReturn> for u32 {
    fn from(ret: LfReturn) -> Self {
        ret.0 as u32
    }
}

impl LfReturnable for u64 {
    fn lf_type() -> _lf_type { LF_TYPE_U64 }
}

impl From<LfReturn> for u64 {
    fn from(ret: LfReturn) -> Self { ret.0 as u64 }
}

/// Invokes a remote function call to a Flipper device.
///
/// All functions belong to a module, whether it's a Standard Module or a
/// User Module. The module's FFI representation must be given in order
/// for libflipper to find the module where the function resides.
/// Within a module, functions are ordered by index, so the index of the
/// desired function within the given module must be given. Then, the
/// desired arguments with which to execute the function must be given.
/// The arguments are type-sensitive and order-sensitive. Finally, the
/// return type of `lf_invoke` must be bound with a type parameter, so
/// that the FMR system can know what type to deliver back to Rust.
///
/// Consider the following C function, which belongs to a Flipper module.
///
/// ```c
/// uint8_t foo(uint16_t bar, uint32_t baz, uint64_t qux);
/// ```
///
/// To execute this function using `lf_invoke` would look like this:
///
/// ```
/// use flipper::{ModuleFFI, UserModuleFFI};
/// use flipper::fmr::{Args, lf_invoke};
///
/// // Don't do this, this is just to get the doc tests to compile.
/// // See how to create a user module.
/// let ffi = ModuleFFI::User(UserModuleFFI::uninitialized("some_module"));
///
/// let args = Args::new()
///                .append(10 as u16)  // bar
///                .append(20 as u32)  // baz
///                .append(30 as u64); // qux
///
/// let output: u8 = lf_invoke(lf_get_current_device(), &ffi, 0, args);
/// ```
pub fn lf_invoke<'a, T: LfReturnable>(module: &'a ModuleFFI, index: u8, args: Args) -> T {
    unsafe {
        let mut arglist: *mut _lf_ll = ptr::null_mut();
        for arg in args.iter() {
            libflipper::lf_ll_append(&mut arglist, &arg.0 as *const _lf_arg as *const c_void, ptr::null());
        }
        let ret = libflipper::lf_invoke(lf_get_current_device(), module.as_ptr(), index, T::lf_type(), arglist);
        T::from(LfReturn(ret))
    }
}

/// Invokes a remote function call to a Flipper device, passing a buffer of
/// data with it.
///
/// This is currently only used for certain Standard Modules such as uart0
/// for sending and receiving data over a bus. However, it may be expanded
/// in the future to support user module functions as well.
pub fn lf_push<'a, T: LfReturnable>(module: &'a ModuleFFI, index: u8, data: &[u8], args: Args) -> T {
    unsafe {
        let mut arglist: *mut _lf_ll = ptr::null_mut();
        for arg in args.iter() {
            libflipper::lf_ll_append(&mut arglist, &arg.0 as *const _lf_arg as *const c_void, ptr::null());
        }
        let ret = libflipper::lf_push(lf_get_current_device(), module.as_ptr(), index, data.as_ptr() as *const c_void, data.len() as u32, arglist);
        T::from(LfReturn(ret))
    }
}

/// Invokes a remote function call to a Flipper device, pulling a buffer of
/// data back upon return.
///
/// This is currently only used for certain Standard Modules such as uart0
/// for sending and receiving data over a bus. However, it may be expanded
/// in the future to support user module functions as well.
pub fn lf_pull<'a, T: LfReturnable>(module: &'a ModuleFFI, index: u8, buffer: &mut [u8], args: Args) -> T {
    unsafe {
        let mut arglist: *mut _lf_ll = ptr::null_mut();
        for arg in args.iter() {
            libflipper::lf_ll_append(&mut arglist, &arg.0 as *const _lf_arg as *const c_void, ptr::null());
        }
        let ret = libflipper::lf_pull(lf_get_current_device(), module.as_ptr(), index, buffer.as_mut_ptr() as *mut c_void, buffer.len() as u32, arglist);
        T::from(LfReturn(ret))
    }
}

mod test {
    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_arg() {
        let argu8 = Arg::from(123u8);
        let argu8_native = argu8.0;
        assert_eq!(argu8_native.arg_type, LF_TYPE_U8);
        assert_eq!(argu8_native.arg_value, 123u64);

        let argu16 = Arg::from(234u16);
        let argu16_native = argu16.0;
        assert_eq!(argu16_native.arg_type, LF_TYPE_U16);
        assert_eq!(argu16_native.arg_value, 234u64);

        let argu32 = Arg::from(345u32);
        let argu32_native = argu32.0;
        assert_eq!(argu32_native.arg_type, LF_TYPE_U32);
        assert_eq!(argu32_native.arg_value, 345u64);
    }

    #[test]
    fn test_arg_builder() {
        let args = Args::new()
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

        for (actual, expected) in args.iter().zip(expected) {
            assert_eq!(actual.0, expected);
        }
    }
}