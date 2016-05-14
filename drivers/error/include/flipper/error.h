#ifndef __error_h__
#define __error_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Expose a defined type for the size of an error code. */
typedef uint16_t lf_error_t;

/* Define each error code. */
enum {
	E_OK,
	E_FMR_PACKET_CRC,
	E_NO_MEM,
	E_TOO_BIG,
	E_FVM_LOAD,
	E_FVM_SYM,
	E_OPEN_SOCK,
	E_CONN_SOCK,
	E_FLIPPER_UNBOUND,
	E_FLIPPER_NOT_FOUND,
	E_HID_MANAGER,
	E_HID_NO_DEV,
	E_HID_TOO_MANY,
	E_HID_OPEN_DEV,
	E_HID_DISCONN_DEV,
	E_HID_WRITE,
	E_HID_TIMEOUT,
	E_IOKIT_DICT,
	E_DL_NOT_FOUND,
	E_DL_LOAD,
	E_DL_LOADED,
	E_FS_OPEN,
	E_FS_ADD_LEAF,
	E_FS_NO_LEAF,
	E_UNIMPLEMENTED,
	E_GREATEST
};

/* Define the strings that correspond to each error code. */
#define E_OK_S                  "No error."
#define E_FMR_PACKET_CRC_S      "FMR packet failed checksum."
#define E_NO_MEM_S              "Out of memory."
#define E_TOO_BIG_S             "Too many arguments."
#define E_FVM_LOAD_S            "FVM already loaded."
#define E_FVM_SYM_S             "FVM symbol not found."
#define E_OPEN_SOCK_S           "Unable to open socket."
#define E_CONN_SOCK_S           "Unable to connect socket."
#define E_FLIPPER_UNBOUND_S     "No device present."
#define E_FLIPPER_NOT_FOUND_S   "Device not found for name given."
#define E_HID_MANAGER_S         "HID manager could not be loaded."
#define E_HID_NO_DEV_S          "HID manager could not find the device."
#define E_HID_TOO_MANY_S        "There are too many HID devices attached."
#define E_HID_OPEN_DEV_S        "HID manager unable to open device."
#define E_HID_DISCONN_DEV_S     "Device was disconnected."
#define E_HID_WRITE_S           "HID manager unable to write to device."
#define E_HID_TIMEOUT_S         "HID manager time out."
#define E_IOKIT_DICT_S          "IOKit dictionary error."
#define E_DL_NOT_FOUND_S        "DL not found."
#define E_DL_LOAD_S             "DL load error."
#define E_DL_LOADED_S           "DL already loaded."
#define E_FS_OPEN_S             "No such file."
#define E_FS_ADD_LEAF_S         "Failed to create file."
#define E_FS_NO_LEAF_S          "File doesn't exist."
#define E_UNIMPLEMENTED_S       "Feature not yet implemented."

/* Determine whether or not the current platform supports error strings. */
#ifndef __osmium__
#define ERROR_STRING(...) __VA_ARGS__
#else
#define ERROR_STRING(...) ""
#endif

/* Organizes the error string macros into an array. */
#define ERROR_STRING_ARRAY		E_OK_S,\
								E_FMR_PACKET_CRC_S,\
								E_NO_MEM_S,\
								E_TOO_BIG_S,\
								E_FVM_LOAD_S,\
								E_FVM_SYM_S,\
								E_OPEN_SOCK_S,\
								E_CONN_SOCK_S,\
								E_FLIPPER_UNBOUND_S,\
								E_FLIPPER_NOT_FOUND_S,\
								E_HID_MANAGER_S,\
								E_HID_MANAGER_S,\
								E_HID_NO_DEV_S,\
								E_HID_TOO_MANY_S,\
								E_HID_OPEN_DEV_S,\
								E_HID_DISCONN_DEV_S,\
								E_HID_WRITE_S,\
								E_HID_TIMEOUT_S,\
								E_IOKIT_DICT_S,\
								E_DL_NOT_FOUND_S,\
								E_DL_LOAD_S,\
								E_FS_OPEN_S,\
								E_FS_ADD_LEAF_S,\
								E_FS_NO_LEAF_S,\
								E_UNIMPLEMENTED_S

/* Declare the virtual interface for this module. */
extern const struct _error {

	/* Configures the error module. */
	void (*configure)(void);

	/* Causes errors to produce side effects, exiting if fatal. */
	void (*withold)(void);

	/* Prevents errors from producing side effects of any kind. */
	void (*disclose)(void);

	/* Generates an error accross the entire FMR heirarchy. */
	void (*raise)(lf_error_t code, char *format, ...) __attribute__ ((format (printf, 2, 3)));

	/* Returns the current error (if any). */
	lf_error_t (* get)(void);

	/* Returns the device to an error-free state. */
	void (*clear)(void);

} error;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _error_configure, _error_withold, _error_disclose, _error_raise, _error_get, _error_clear };

/* Declare each prototype for all functions within this driver. */
extern void error_configure(void);
extern void error_withold(void);
extern void error_disclose(void);
extern void error_raise(lf_error_t code, char *format, ...) __attribute__ ((format (printf, 2, 3)));
extern lf_error_t error_get(void);
extern void error_clear(void);

/* Global state that describes whether or not errors should produce side effects. */
extern uint8_t error_disclosed;

/* Global state that contains the error code. */
extern lf_error_t error_code;

/* Global state that enumerates the strings corresponding to each error code for the current platform (if supported). */
extern char *error_messages[];

#endif
#endif
