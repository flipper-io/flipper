#![allow(non_camel_case_types)]
#![allow(non_snake_case)]
#![deny(unstable_features)]
#![deny(unused_import_braces)]
#![deny(unused_qualifications)]
#![deny(warnings)]

#[allow(unused_imports)]
#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;
extern crate libc;

pub mod fsm;
pub mod fmr;

use std::ptr;
use std::ffi::CString;
use libc::{c_void, c_char, c_int};

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

/// Contains the information necessary to interact with a standard module within
/// libflipper. This is a thin wrapper for the purpose of hiding the underlying
/// representation.
pub struct StandardModuleFFI {
    /// A pointer to a standard module within libflipper.
    module_meta: *const _lf_module,
}

/// Contains owned metadata about a user module in the proper format for
/// interacting with libflipper.
pub struct UserModuleFFI {
    /// Since we assign the name of the module before binding it with
    /// lf_bind, we use a CString to back the memory of the string.
    /// By storing the CString adjacent to the _lf_module in the same
    /// struct, we ensure that the string lives as long as
    /// the _lf_module.
    _name: CString,
    module_meta: _lf_module,
}

impl<'a> From<(&'a str, u16, u16, u16)> for UserModuleFFI {
    fn from(vals: (&'a str, u16, u16, u16)) -> Self {
        let (name, version, crc, index) = vals;
        let string = CString::new(name).unwrap();
        let string_ref = string.as_ptr();
        UserModuleFFI {
            _name: string,
            module_meta: _lf_module {
                name: string_ref,
                description: ptr::null(),
                version,
                crc,
                index,
                device: ptr::null(),
            },
        }
    }
}

impl UserModuleFFI {
    /// User module metadata is owned by the rust bindings. In order to
    /// instantiate a user module, one creates an uninitialized owned
    /// representation of the user module with nothing but the name
    /// populated inside. This uninitialized but named metadata struct
    /// is then passed to `lf_bind`, where libflipper then initializes
    /// all of the module metadata.
    pub fn uninitialized(name: &str) -> UserModuleFFI {
        UserModuleFFI::from((name, 0, 0, 0))
    }
}

/// A unified representation of standard and user modules for simplifying
/// the interaction between module bindings and libflipper.
pub enum ModuleFFI {
    Standard(StandardModuleFFI),
    User(UserModuleFFI),
}

impl ModuleFFI {
    fn as_ptr(&self) -> *const _lf_module {
        match *self {
            ModuleFFI::Standard(ref standard) => standard.module_meta,
            ModuleFFI::User(ref user) => &user.module_meta,
        }
    }
}

/// Standard Modules are modules that are built into Flipper and always
/// available on the device.
///
/// Standard Modules must implement `new` and `bind`. The `new` constructor
/// gives an instance of the module that is attached to the "default"
/// Flipper. `bind` gives an instance which is attached to a specific
/// Flipper.
pub trait StandardModule {
    fn new() -> Self;
    fn bind(flipper: &Flipper) -> Self;
}

/// User Modules are modules which are written in C by the user.
///
/// When a user creates a module, they give it a name which is used by
/// Flipper to load and bind to it. To use it from the Rust bindings,
/// the user must specify the name so that rust can find the module.
pub trait UserModule<'a>: From<(&'a Flipper, UserModuleFFI)> {
    const NAME: &'a str;

    /// Binds an instance of a User Module to the given Flipper.
    ///
    /// ```rust,no_run
    /// use flipper::{Flipper, UserModule, ModuleFFI, UserModuleFFI};
    ///
    /// struct MyModule<'a> {
    ///     flipper: &'a Flipper,
    ///     module: ModuleFFI,
    /// }
    ///
    /// impl<'a> UserModule<'a> for MyModule<'a> {
    ///     const NAME: &'a str = "My module";
    ///     fn new(flipper: &'a Flipper) -> Self {
    ///         MyModule {
    ///             flipper,
    ///             module: ModuleFFI::User(UserModuleFFI::uninitialized(Self::NAME)),
    ///         }
    ///     }
    /// }
    ///
    /// impl<'a> From<(&'a Flipper, UserModuleFFI)> for MyModule<'a> {
    ///     fn from((flipper, module): (&'a Flipper, UserModuleFFI)) -> Self {
    ///         MyModule {
    ///             flipper,
    ///             module: ModuleFFI::User(module),
    ///         }
    ///     }
    /// }
    ///
    /// impl<'a> MyModule<'a> {
    ///     fn myFunc(&self) {
    ///         // Do FMR invocation
    ///     }
    /// }
    ///
    /// let flipper = Flipper::attach();
    /// let myModule = MyModule::new(&flipper); // Binds "MyModule" to control "flipper".
    ///
    /// my_module.my_func();
    /// ```
    fn new(flipper: &'a Flipper) -> Self {
        let mut module = UserModuleFFI::uninitialized(Self::NAME);
        unsafe { lf_bind(flipper.device, &mut module.module_meta); }
        Self::from((flipper, module))
    }
}

#[link(name = "flipper")]
extern {
    fn flipper_attach() -> _lf_device;
    fn carbon_attach_hostname(hostname: *const c_char) -> _lf_device;
    fn lf_bind(device: *const c_void, module: *mut _lf_module) -> c_int;
}

pub struct Flipper {
    /// A reference to an active Flipper profile in libflipper. This
    /// is used when communicating with libflipper to specify which
    /// device functions should be executed on.
    device: _lf_device,
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
            let hostname_CString = CString::new(hostname).unwrap();
            let device = carbon_attach_hostname(hostname_CString.as_ptr());
            if device == ptr::null() { return Err(FlipperError::Attach); }
            Ok(Flipper { device })
        }
    }

    pub fn select_u2_gpio(&self) {
        unsafe { carbon_select_u2_gpio(self.device) };
    }
}
