/* fmr.h - Flipper Message Runtime macros, types, and implementation. */

#ifndef __lf_fmr_h__
#define __lf_fmr_h__

#include <flipper/types.h>
#include <flipper/error.h>
#include <flipper/ll.h>

/* The size of a single FMR packet expressed in bytes. */
#define FMR_PACKET_SIZE 64
/* The magic number that indicates the start of a packet. */
#define FMR_MAGIC_NUMBER 0xFE

/* ~ Define types exposed by the FMR API. ~ */

/* The variadic argument type. Used to hold argument metadata and value during parsing. */
typedef uint64_t fmr_va;
/* The largest argument type. All argument values are held within a variable of this type. */
typedef uint64_t fmr_arg;
/* Used to hold the index of a standard or user module in which a function counterpart exists. */
typedef uint32_t fmr_module;
/* Used to hold the offset index of a function within a module. */
typedef uint8_t fmr_function;
/* Used to hold the number of parameters that are to be passed during a procedure call. */
typedef uint8_t fmr_argc;

/* The maximum number of arguments that can be encoded into a packet. */
#define FMR_MAX_ARGC 16
/* Used to hold encoded prameter types within invocation metadata.
   NOTE: This type must be capable of encoding the exact number of bits
		 given by (FMR_MAX_ARGC * 2).
*/
typedef uint32_t fmr_types;

/* Converts a C type into an unsigned fmr_type. */
#define fmr_utype(type) (sizeof(type) - 1)
/* Converts a C type into a signed fmr_type. */
#define fmr_stype(type) ((1 << 3) | fmr_utype(type))
/* Calculates the length of an FMR type. */
#define fmr_sizeof(type) ((type != fmr_void_t && type != fmr_int_t && type != fmr_ptr_t) ? ((type & 0x7) + 1) : 8)

/* Enumerates the basic type signatures an argument can be classified as. */
enum {

	fmr_void_t = 2,                     // 2
	fmr_int_t = 4,                      // 4
	fmr_ptr_t = 6,                      // 6

	/* Unsigned types. */
	fmr_uint8_t = fmr_utype(uint8_t),   // 0
	fmr_uint16_t = fmr_utype(uint16_t), // 1
	fmr_uint32_t = fmr_utype(uint32_t), // 3
	fmr_uint64_t = fmr_utype(uint64_t), // 7

	/* Signed types. */
	fmr_int8_t = fmr_stype(int8_t),     // 8
	fmr_int16_t = fmr_stype(int16_t),   // 9
	fmr_int32_t = fmr_stype(int32_t),   // 11
	fmr_int64_t = fmr_stype(int64_t),   // 15

	/* Max type is 15. */
	fmr_max_t = 15
};

/* A type used to reference the values in the enum above. */
typedef uint8_t fmr_type;

/* ~ Parameter list building macros. */

/* Counts the number of arguments within a variadic argument macro. */
#define __fmr_count_implicit(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _a, _b, _c, _d, _e, _f, _10, n, ...) n
#define __fmr_count(...) __fmr_count_implicit(_, ##__VA_ARGS__, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)

/* Generates and returns a pointer to an 'fmr_parameters' given a list of variadic arguments. */
#define fmr_args(...) fmr_build((__fmr_count(__VA_ARGS__)/2), ##__VA_ARGS__)

/* ~ Parser macros for variables. */

/* Creates an 'fmr_va' from an 'fmr_type' and an immediate value. */
#define fmr_intx(type, arg) (fmr_type)type, (fmr_arg)arg
/* Gives the 'fmr_va' for a given 8-bit integer's value. */
#define fmr_int8(arg) fmr_intx(fmr_int8_t, (uint8_t)arg)
/* Gives the 'fmr_va' for a given 16-bit integer's value. */
#define fmr_int16(arg) fmr_intx(fmr_int16_t, (uint16_t)arg)
/* Gives the 'fmr_va' for a given 32-bit integer's value. */
#define fmr_int32(arg) fmr_intx(fmr_int32_t, (uint32_t)arg)
/* Gives the 'fmr_va' for a pointer. */
#define fmr_ptr(arg) fmr_intx(fmr_ptr_t, (uintptr_t)arg)
/* Creates an 'fmr_va' from a C variable. */
#define fmr_infer(variable) fmr_intx(fmr_utype(variable), variable)

/* If this bit is set in the module's index, then it is a user module. */
#define FMR_USER_INVOCATION_BIT (1 << 8)

/* Exposes all message runtime packet classes. */
enum {
	/* Invokes a function in a standard module. */
	fmr_standard_invocation_class,
	/* Invokes a function in user module. */
	fmr_user_invocation_class,
	/* Causes a push operation to begin. */
	fmr_push_class,
	/* Causes a pull operation to begin. */
	fmr_pull_class,
	/* Sends data to device. */
	fmr_send_class,
	/* Receives data from the device. */
	fmr_receive_class,
	/* Experimental: Caused a RAM load and launch. */
	fmr_ram_load_class,
	/* Signals the occurance an event. */
	fmr_event_class
};

/* A type used to reference the values in the enum above. */
typedef uint8_t fmr_class;

/* Contains the information required to obtain, verify, and parse a packet. */
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
struct _lf_arg {
	/* The type signature of the argument. */
	fmr_type type;
	/* The value of the argument. */
	fmr_arg value;
};

/* Generic packet data type that can be passed around by packet parsing equipment. */
struct LF_PACKED _fmr_packet {
	/* The header shared by all packet classes. */
	struct _fmr_header header;
	/* A generic payload that is designed to be casted against the class specific data structures. */
	uint8_t payload[(FMR_PACKET_SIZE - sizeof(struct _fmr_header))];
};

/* Procedure call metadata carried by a packet. */
struct LF_PACKED _fmr_invocation {
	/* The index of the module in which the target routine resides. */
	uint8_t index;
	/* The index of the function within the module. */
	uint8_t function;
	/* The return type. */
	fmr_type ret;
	/* The types of the encoded parameters. */
	fmr_types types;
	/* The number of encoded parameters. */
	fmr_argc argc;
	/* The encoded values of the parameters to be passed to the callee. */
	uint8_t parameters[];
};

/* Contains metadata needed to perform a remote procedure call on a device. */
struct LF_PACKED _fmr_invocation_packet {
	/* The packet header programmed with 'fmr_standard_invocation_class' or 'fmr_user_invocation_class'. */
	struct _fmr_header header;
	/* The procedure call information of the invocation. */
	struct _fmr_invocation call;
};

/* Contains metadata needed to perform a push/pull operation. */
struct LF_PACKED _fmr_push_pull_packet {
	/* The packet header programmed with 'fmr_push_class' or 'fmr_pull_class'. */
	struct _fmr_header header;
	/* The amount of data to be transferred. */
	lf_size_t length;
	/* The procedure call information of the invocation. */
	struct _fmr_invocation call;
};

/* A generic datastructure that is sent back following any message runtime transaction. */
struct LF_PACKED _fmr_result {
	/* The return value of the function called (if any). */
	lf_return_t value;
	/* The error code generated on the device. */
	lf_error_t error;
	/* NOTE: Add bitfield indicating the need to poll for updates. */
};

/* A reference to the fmr_modules array. */
extern const void *const fmr_modules[];

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */

/* Builds an fmr_parameters from a set of variadic arguments provided by the fmr_parameters macro. */
struct _lf_ll *fmr_build(int argc, ...);
/* Appends an argument to an fmr_parameters. */
int fmr_append(struct _lf_ll *list, fmr_type type, fmr_arg value);
/* Generates the appropriate data structure needed for the remote procedure call of 'funtion' in 'module'. */
int fmr_create_call(fmr_module module, fmr_function function, fmr_type ret, struct _lf_ll *args, struct _fmr_header *header, struct _fmr_invocation *call);
/* Executes a standard module. */
lf_return_t fmr_execute(fmr_module module, fmr_function function, fmr_type ret, fmr_argc argc, fmr_types argt, void *arguments);
/* Executes an fmr_packet and stores the result of the operation in the result buffer provided. */
int fmr_perform(struct _fmr_packet *packet, struct _fmr_result *result);

/* Helper function for lf_push. */
extern lf_return_t fmr_push(struct _fmr_push_pull_packet *packet);
/* Helper function for lf_pull. */
extern lf_return_t fmr_pull(struct _fmr_push_pull_packet *packet);

/* ~ Functions with platform specific implementation. ~ */

/* Unpacks the argument buffer into the CPU following the native architecture's calling convention and jumps to the given function pointer. */
extern lf_return_t fmr_call(lf_return_t (* function)(void), fmr_type ret, uint8_t argc, uint16_t argt, void *argv);

#endif
