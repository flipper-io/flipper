/* fmr.h - Flipper Message Runtime macros, types, and implementation. */

#ifndef __lf_fmr_h__
#define __lf_fmr_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Include all supporting header files. */
#include <flipper/carbon/modules/error.h>

/* Defines the size (in bytes) of a single FMR packet. */
#define FMR_PACKET_SIZE 64
/* Define the upper limit of the number of arguments a function can be invoked using. */
#define FMR_MAX_ARGC 16
/* The magic number. */
#define FMR_MAGIC_NUMBER 0xFE

/* Define types exposed by the FMR API. */
typedef uint64_t fmr_va;
typedef uint32_t fmr_arg;
typedef lf_crc_t fmr_module;
typedef uint8_t fmr_function;
typedef uint8_t fmr_argc;
typedef uint16_t fmr_types;
typedef uint32_t fmr_return;

/* Enumerates the basic type signatures an argument can be classified using. */
typedef enum {
	/* Explicit argument types. */
	fmr_int8_t,
	fmr_int16_t,
	fmr_int32_t
} fmr_type;

/* Counts the number of arguments inside a variadic argument macro. */
#define __fmr_count_implicit(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _a, _b, _c, _d, _e, _f, _10, n, ...) n
#define __fmr_count(...) __fmr_count_implicit(_, ##__VA_ARGS__, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)

/* Provides a wrapper around fmr_build and fmr_list. */
#define fmr_args(...) fmr_build(__fmr_count(__VA_ARGS__), ##__VA_ARGS__)

/* Explicitly describes the width of an argument to the argument parser. */
#define fmr_intx(type, arg) (((fmr_va)type << (sizeof(fmr_arg) * 8)) | (fmr_arg)arg)
#define fmr_int8(arg) fmr_intx(fmr_int8_t, (uint8_t)arg)
#define fmr_int16(arg) fmr_intx(fmr_int16_t, (uint16_t)arg)
#define fmr_int32(arg) fmr_intx(fmr_int32_t, (uint32_t)arg)
/* Converts a C type into a message runtime type. */
#define fmr_type(type) (sizeof(type) >> 1)
/* Converts a C type and value into a message runtime type and value. */
#define fmr_cast(type, arg) fmr_intx(fmr_type(type), arg)
/* Creates a message runtime argument via type inference. */
#define fmr_infer(arg) fmr_cast(arg, arg)

/* Calculates the length of an FMR type. */
#define fmr_sizeof(type) (1 << type)

/* Exposes all message runtime packet classes. */
enum {
	/* Asks a device for its configuration information. */
	fmr_configuration_class,
	/* Invokes a function in a standard module. */
	fmr_standard_invocation_class,
	/* Invokes a function in user module. */
	fmr_user_invocation_class,
	/* Signals the occurance an event. */
	fmr_event_class
};

typedef uint8_t fmr_class;

/* Contains the information required to obtain, verify, and route the packet. */
struct LF_PACKED _fmr_header {
	/* A magic number indicating the start of the packet. */
	uint8_t magic;
	/* The checksum of the packet's contents. */
	lf_crc_t checksum;
	/* The length of the packet expressed in bytes. */
	uint16_t length;
	/* The packet's class. */
	fmr_class class;
};

/* Standardizes the notion of an argument. */
struct _fmr_arg {
	/* The value of the argument. */
	fmr_arg value;
	/* The type signature of the argument. */
	fmr_type type;
	/* The next argument. */
	struct _fmr_arg *next;
};

/* Organizes arguments in a format passible via pointer. */
struct _fmr_list {
	/* The number of arguments contained in the list. */
	fmr_argc argc;
	/* A pointer to the first argument in the list. */
	struct _fmr_arg *argv;
};

#define LF_CONFIGURATION 0x20
#define LF_STANDARD_MODULE 0x80
#define LF_PUSH_PULL_FUNCTION 0x40

/* Generic packet data type that can be passed around by packet parsing equipment. */
struct LF_PACKED _fmr_packet {
	/* The header shared by all packet classes. */
	struct _fmr_header header;
	/* A generic payload that is designed to be casted against the class specific data structures. */
	uint8_t payload[(FMR_PACKET_SIZE - sizeof(struct _fmr_header))];
};

/* Contains metadata needed to query a device about its configuration. */
struct LF_PACKED _fmr_configuration_packet {
	/* The packet header programmed with 'fmr_configuration_class'. */
	struct _fmr_header header;
	/* Padding for the unused section of the packet. */
	uint8_t padding[(sizeof(struct _fmr_packet) - sizeof(struct _fmr_header))];
};

/* Contains metadata needed to perform a remote procedure call on a device. */
struct LF_PACKED _fmr_invocation_packet {
	/* The packet header programmed with 'fmr_standard_invocation_class' or 'fmr_user_invocation_class'. */
	struct _fmr_header header;
	/* The procedure call information carried by the invocation packet. */
	struct LF_PACKED _fmr_call {
		/* The index of the module in which the target routine resides. */
		uint8_t index;
		/* The index of the function within the module. */
		uint8_t function;
		/* The number of parameters encoded into the packet. */
		fmr_argc argc;
	} call;
	/* The encoded values of the parameters to be passed to the callee. */
	uint8_t parameters[(sizeof(struct _fmr_packet) - sizeof(struct _fmr_header) - sizeof(struct _fmr_call))];
};

/* A generic datastructure that is sent back following any message runtime transaction. */
struct LF_PACKED _fmr_result {
	/* The return value of the function called (if any). */
	fmr_arg value;
	/* The error code generated on the device. */
	lf_error_t error;
	/* NOTE: Add bitfield indicating the need to poll for updates. */
};

/* Declare the virtual interface for this module. */
extern const struct _fmr {
	/* Configures the button hardware. */
	void (* push)(fmr_module module, fmr_function function, lf_size_t length);
	/* Reads back the button state; returns 0 when released and 1 when pressed. */
	void (* pull)(fmr_module module, fmr_function function, lf_size_t length);
} fmr;

#ifdef __private_include__

/* A reference to the fmr_modules array. */
extern const void *const fmr_modules[];

/* The fmr_module structure for this module. */
extern struct _lf_module _fmr;

/* ~ Declare the FMR overlay for this driver. ~ */
enum { _fmr_push, _fmr_pull };

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */

/* Builds an fmr_list from a set of variadic arguments provided by the fmr_list macro. */
struct _fmr_list *fmr_build(fmr_argc argc, ...);
/* Appends an argument to an fmr_list. */
void fmr_append(struct _fmr_list *list, struct _fmr_arg *argument);
/* Concatenates two argument lists. */
struct _fmr_list *fmr_merge(struct _fmr_list *first, struct _fmr_list *second);
/* Removes and returns the item at the top of the list. */
struct _fmr_arg *fmr_pop(struct _fmr_list *list);
/* Frees an fmr_list. */
int fmr_free(struct _fmr_list *list);
/* Generates the appropriate data structure needed for the remote procedure call of 'funtion' in 'module'. */
int fmr_generate(fmr_module module, fmr_function function, struct _fmr_list *args, struct _fmr_invocation_packet *packet);
/* Executes a standard module. */
fmr_return fmr_execute(fmr_module module, fmr_function function, fmr_argc argc, fmr_types types, void *arguments);
/* Executes an fmr_packet and stores the result of the operation in the result buffer provided. */
int fmr_perform(struct _fmr_packet *packet, struct _fmr_result *result);

/* Helper function for lf_push. */
void fmr_push(fmr_module module, fmr_function function, lf_size_t length);
/* Helper function for lf_pull. */
void fmr_pull(fmr_module module, fmr_function function, lf_size_t length);

/* ~ Functions with platform specific implementation. ~ */

/* Abstracts platform specific implementation needed to access the standard module array. */
extern const void *lf_std_function(fmr_module module, fmr_function function);
/* Unpacks the argument buffer into the CPU following the native architecture's calling convention and jumps to the given function pointer. */
extern uint32_t fmr_call(const void *function, uint8_t argc, uint16_t argt, void *argv);

#endif
#endif
