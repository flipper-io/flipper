use std::result;
use std::os::raw::c_char;

type lf_error_t = i32;

extern {
    fn lf_error_configure() -> i32;
    fn lf_error_raise(err: lf_error_t, format: *const c_char, ...);
    fn lf_error_string() -> *const c_char;
    fn lf_error_resume();
    fn lf_error_pause();
    fn error_get() -> lf_error_t;
    fn lf_error_clear();
}

enum Error {
    Malloc,
    Null,
    Overflow,
    NoDevice,
    NotAttached,
    AlreadyAttached,
    FsExists,
    FsNoFile,
    FmrOverflow,
    Fmr,
    Endpoint,
    LibUsb,
    Communication,
    Socket,
    Module,
    Resolution,
    String,
    Checksum,
    Name,
    Configuration,
    Ack,
    Type,
    Boundary,
    Timer,
    Timeout,
    NoPID,
    InvalidTask,
    Subclass,
    Unimplemented,
}

type Result<T> = result::Result<T, Error>;
