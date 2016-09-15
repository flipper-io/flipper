/* fmr.h - Flipper Message Runtime macros, types, and implementation. */

#ifndef __lf_fmr_h__
#define __lf_fmr_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Include all supporting header files. */
#include <flipper/error.h>

/* Defines the size (in bytes) of a single FMR packet. */
#define FMR_PACKET_SIZE 64

/* Define the upper limit of the number of arguments a function can be invoked using. */
#define FMR_MAX_ARGC 16

/* Define types exposed by the FMR API. */
typedef uint64_t fmr_va;
typedef uint32_t fmr_arg;
typedef uint8_t fmr_argc;
typedef uint8_t fmr_function;
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
#define fmr_intx(type, arg) (((fmr_va)type << (sizeof(fmr_arg) * 8)) | arg)
#define fmr_int8(arg) fmr_intx(fmr_int8_t, (uint8_t)arg)
#define fmr_int16(arg) fmr_intx(fmr_int16_t, (uint16_t)arg)
#define fmr_int32(arg) fmr_intx(fmr_int32_t, (uint32_t)arg)
/* Wrappers around the above using C names. */
#define fmr_char(arg) fmr_int8(arg)
#define fmr_short(arg) fmr_int16(arg)
#define fmr_int(arg) fmr_int32(arg)

/* Calculates the length of an FMR type. */
#define fmr_sizeof(type) (1 << type)

/* Standardizes the notion of a module. */
struct _fmr_module {
	/* A string containing the module's name. */
	char *name;
	/* A string giving the description of a module. */
	char *description;
	/* The version of the module. */
	lf_version_t version;
	/* The module's identifier. */
	lf_id_t identifier;
	/* The device upon which the module's counterpart is located. */
	struct _lf_device *device;
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

/* Contains the information required to obtain and verify the packet body. */
struct __attribute__((__packed__)) _fmr_header {
	/* A magic number indicating the start of the packet. */
	uint8_t magic;
	/* The checksum of the packet's contents. */
	lf_id_t checksum;
	/* The length of the packet expressed in bytes. */
	uint16_t length;
};

/* Describes the target module with which the packet will be interacting.  */
struct __attribute__((__packed__)) _fmr_target {
	/* The identifier of the module. */
	lf_id_t module;
	/* The identifier of the function or variable. */
	fmr_function function;
	/* The number of arguments encoded in the packet. */
	fmr_argc argc;
};

/* Organizes the sub-components of an FMR packet into a single data structure. */
struct __attribute__((__packed__)) _fmr_packet {
	struct _fmr_header header;
	struct _fmr_target target;
	uint8_t body[(FMR_PACKET_SIZE - sizeof(struct _fmr_header) - sizeof(struct _fmr_target))];
};

/* Describes the results obtained from parsing a packet. */
struct _fmr_result {
	/* The return value of the function called (if any). */
	fmr_arg value;
	/* The error code generated as a result of the parse (if any). */
	lf_error_t error;
};

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */

/* Builds an fmr_list from a set of variadic arguments provided by the fmr_list macro. */
struct _fmr_list *fmr_build(fmr_argc argc, ...);
/* Appends an argument to an fmr_list. */
void fmr_append(struct _fmr_list *list, fmr_type type, fmr_va value);
/* Removes and returns the item at the top of the list. */
struct _fmr_arg *fmr_pop(struct _fmr_list *list);
/* Frees an fmr_list. */
void fmr_free(struct _fmr_list *list);
/* Binds a module to its counterpart the selected Flipper device. */
int fmr_bind(struct _fmr_module *module, char *name);
int fmr_generate(struct _fmr_module *module, fmr_function function, struct _fmr_list *args, struct _fmr_packet *packet);
/* Parses an fmr_packet and generates the necessary side effects. */
void fmr_parse(struct _fmr_packet *packet);

/* Unpacks argument types and calls a function pointer per the given architecture's calling convention. */
extern uint32_t fmr_call(void *function, uint8_t argc, uint16_t argt, void *argv);

#endif
#endif
