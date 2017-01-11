extern crate libc;
use std::ffi::CStr;
use std::str;

#[repr(C)]
struct _lf_configuration {
    name: [libc::c_char],
}

#[repr(C)]
struct _lf_device {
    configuration: _lf_configuration,
}

pub struct LFDevice {
    /* Pointer to the _lf_device struct. */
    pointer: *mut _lf_device,
}

impl LFDevice {
    /* Returns the name of the device as a string slice. */
    pub fn name(&self) -> &str {
        let c_buf: *const libc::c_char = unsafe { (*self.pointer).configuration.name.as_ptr() };
        let c_str: &CStr = unsafe { CStr::from_ptr(c_buf) };
        let buf: &[u8] = c_str.to_bytes();
        return str::from_utf8(buf).unwrap();
    }
}

pub mod led;

#[link(name = "flipper")]
extern {
    fn flipper_attach() -> *mut _lf_device;
    fn flipper_select(device: *mut _lf_device) -> i32;
    fn flipper_detach(device: *mut _lf_device) -> i32;
}

pub fn attach() -> LFDevice {
    unsafe {
        return LFDevice { pointer: flipper_attach() };
    };
}

pub fn select(device: &LFDevice) {
    unsafe {
        flipper_select(device.pointer);
    };
}

impl Drop for LFDevice {
  fn drop(&mut self) {
    unsafe {
        flipper_detach(self.pointer);
    }
  }
}
