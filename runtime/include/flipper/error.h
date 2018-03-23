/* error.h - Standardizes the notion of error handling across all attached devices. */

#ifndef __lf_error_h__
#define __lf_error_h__

/* Include all types exposed by libflipper. */
#include <flipper/types.h>

/* Success code macros. */
#define lf_success 0
#define lf_error -1

/* Short hand for raising errors based on the truth of a condition. */
#define lf_assert(truth, label, error, ...) \
	if (!(truth)) { \
		lf_error_raise(error, error_message(__VA_ARGS__)); \
		goto label; \
	}

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
	E_TIMER,
	E_TIMEOUT,
	E_NO_PID,
	E_INVALID_TASK,
	E_SUBCLASS,
	E_UNIMPLEMENTED,
	E_TEST,

	/* uart0 */
	E_UART0_PUSH_TIMEOUT,
	E_UART0_PULL_TIMEOUT,

    E_MAX
};

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
								 "acknowledgement error", \
								 "type error", \
								 "boundary error", \
								 "timer error", \
								 "timeout error", \
								 "no task for pid", \
								 "invalid task specified", \
								 "packet subclass error", \
								 "unimplemented error",	\
								 "test failed", \
								 "uart0 push timeout", \
								 "uart0 pull timeout"


/* If this flag is set, error messages are nullified on platforms that do not need to store error strings. */
#ifndef __no_err_str__
/* Allow the 'error_message' macro to serve as a passthrough for any variadic arguments supplied to it. */
#define error_message(...) __VA_ARGS__
#else
/* Define the 'error_message' macro as NULL to prevent memory from being wasted storing error strings that will never be used.*/
#define error_message(...) NULL
#endif

/* Prevents the execution of a statement from producing error-related side effects. */
#define lf_try(statement) lf_error_pause(); statement; lf_error_resume();

extern int lf_debug_level;

/* Configures the error module. */
extern int lf_error_configure(void);
/* Raises an error internally to the current context of libflipper. */
extern void lf_error_raise(lf_error_t error, const char *format, ...) __attribute__ ((format (printf, 2, 3)));
/* Provide the error message string. */
extern char *lf_error_string(void);
/* Causes errors to resume the producion side effects, exiting if fatal. */
extern void lf_error_resume(void);
/* Pauses errors from producing side effects of any kind. */
extern void lf_error_pause(void);
/* Return the current error state. */
extern lf_error_t lf_error_get(void);
/* Clear the current error state. */
extern void lf_error_clear(void);

#endif
