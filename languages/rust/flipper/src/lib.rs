extern crate libc;
use libc::c_char;
use libc::c_void;
use std::ffi::CStr;
use std::str;
use std::fmt;

pub mod led;
pub mod gpio;

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

pub struct LFDevice {
    /* Reference to the _lf_device struct. */
    _device: *mut _lf_device
}

impl LFDevice {
    /* Returns the name of the device as a string slice. */
    pub fn name(&self) -> &str {
        let c_buf: *const c_char = unsafe { (*self._device).configuration.name.as_ptr() };
        let c_str: &CStr = unsafe { CStr::from_ptr(c_buf) };
        let buf: &[u8] = c_str.to_bytes();
        return str::from_utf8(buf).unwrap();
    }
    pub fn identifier(&self) -> i32 {
        return unsafe { (*self._device).configuration.identifier };
    }
    pub fn version(&self) -> i32 {
        return unsafe { (*self._device).configuration.version };
    }
    pub fn attributes(&self) -> i8 {
        return unsafe { (*self._device).configuration.attributes };
    }
}

impl fmt::Display for LFDevice {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        return write!(f, "name: {}\nidentifier: {:#x}\nversion: {:#x}\nattributes: {:#x}", self.name(), self.identifier(), self.version(), self.attributes());
    }
}

#[link(name = "flipper")]
extern {
    fn flipper_attach() -> *mut _lf_device;
    fn flipper_select(device: *mut _lf_device) -> i32;
    fn flipper_detach(device: *mut _lf_device) -> i32;
}

pub fn attach() -> LFDevice {
    unsafe {
        return LFDevice { _device: flipper_attach() };
    };
}

pub fn select(device: &LFDevice) {
    unsafe {
        flipper_select(device._device);
    };
}

impl Drop for LFDevice {
  fn drop(&mut self) {
    unsafe {
        flipper_detach(self._device);
    }
  }
}
