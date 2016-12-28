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
	E_RESOULTION,
	E_STRING,
	E_CHECKSUM,
	E_NAME,
	E_CONFIGURATION,
	E_ACK,
	E_TYPE,
	E_BOUNDARY,
	E_TIMER
};

/* If this flag is set, error messages are nullified on platforms that do not need to store error strings. */
#ifdef __enable_error_side_effects__
/* These are the error strings that correspond to the values in the error code enumeration. */
#define LF_ERROR_MESSAGE_STRINGS "no error", \
								 "malloc failure", \
								 "null pointer", \
								 "overflow", \
								 "no device", \
								 "device not yet attached", \
								 "device already attached", \
								 "file already exists", \
								 "file does not exist", \
								 "message runtime packet overflow", \
								 "message runtime error", \
								 "endpoint error", \
								 "libusb error", \
								 "communication error", \
								 "socket error", \
								 "no module found", \
								 "address resoultion failure", \
								 "invalid error string", \
								 "checksums do not match", \
								 "invalid name", \
								 "configuration error", \
								 "acknowledgement error" \
								 "type error", \
								 "boundary error"

/* Allow the 'error_message' macro to serve as a passthrough for any variadic arguments supplied to it. */
#define error_message(...) __VA_ARGS__
#else
/* Define the 'error_message' macro as NULL to prevent memory from being wasted storing error strings that will never be used.*/
#define error_message(...) NULL
#endif

/* Prevents the execution of a statement from producing error-related side effects. */
#define suppress_errors(statement) error_pause(); statement; error_resume();

/* Declare the virtual interface for this module. */
extern const struct _error {
	/* Configures the error module. */
	int (* configure)(void);
	/* Causes errors to produce side effects, exiting if fatal. */
	void (* resume)(void);
	/* Prevents errors from producing side effects of any kind. */
	void (* pause)(void);
	/* Generates an error accross the entire FMR heirarchy. */
	void (* raise)(lf_error_t error, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
	/* Returns the current error (if any). */
	lf_error_t (* get)(void);
	/* Returns the device to an error-free state. */
	void (* clear)(void);

} error;

#ifdef __private_include__

/* Configures the error module. */
extern int error_configure(void);
/* Raises an error internally to the current context of libflipper. */
extern void error_raise(lf_error_t error, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
/* Causes errors to resume the producion side effects, exiting if fatal. */
extern void error_resume(void);
/* Pauses errors from producing side effects of any kind. */
extern void error_pause(void);
/* Return the current error state. */
extern lf_error_t error_get(void);
/* Clear the current error state. */
extern void error_clear(void);

#endif
#endif
