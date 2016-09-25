/* error.h - Standardizes the notion of error handling across all attached devices. */

#ifndef __lf_error_h__
#define __lf_error_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Enumerate all of the error codes. */
enum {
	E_OK,
	E_MALLOC,
	E_NULL,
	E_OVERFLOW,
	E_NO_DEVICE,
	E_NOT_ATTACHED,
	E_ALREADY_ATTACHED,
	E_FS_EXISTS,
	E_FS_NO_FILE,
	E_FMR_OVERFLOW,
	E_FMR,
	E_ENDPOINT,
	E_LIBUSB,
	E_COMMUNICATION,
	E_SOCKET,
	E_MODULE,
	E_LAST
};

/* If this flag is set, error messages are nullified on platforms that do not need to store error strings. */
#ifdef __enable_error_side_effects__
/* These are the error strings that correspond to the values in the error code enumeration. */
#define LF_ERROR_MESSAGE_STRINGS "no error", "malloc failure", "null pointer", "overflow", "no such device", "device not yet attached", "device already attached", "file already exists", "file does not exist", "message runtime packet overflow", "message runtime error", "endpoint error", "libusb error", "communication error", "socket error", "no module found"
/* Allow the 'error_message' macro to serve as a passthrough for any variadic arguments supplied to it. */
#define error_message(...) __VA_ARGS__
#else
/* Define the 'error_message' macro as NULL to prevent memory from being wasted storing error strings that will never be used.*/
#define error_message(...) NULL
#endif

/* Prevents the execution of a statement from producing error-related side effects. */
#define suppress_errors(statement) error_pause(); statement; error_resume();

/* Raises an error internally to the current context of libflipper. */
void error_raise(lf_error_t error, const char *format, ...);
/* Causes errors to resume the producion side effects, exiting if fatal. */
void error_resume(void);
/* Pauses errors from producing side effects of any kind. */
void error_pause(void);

#endif
