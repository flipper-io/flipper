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
	E_ENDPOINT,
	E_LIBUSB,
	E_TRANSFER,
	E_SOCKET,
	E_MODULE,
	E_LAST
};

/* Nullifies error messages on platforms that do not need to store them. */
#define error_message(...) __VA_ARGS__

/* Prevents the execution of a statement from producing error-related side effects. */
#define suppress_errors(statement) error_pause(); statement; error_resume();

/* Raises an error internally to the current context of libflipper. */
void error_raise(lf_error_t error, const char *format, ...);
/* Causes errors to resume the producion side effects, exiting if fatal. */
void error_resume(void);
/* Pauses errors from producing side effects of any kind. */
void error_pause(void);

#endif
