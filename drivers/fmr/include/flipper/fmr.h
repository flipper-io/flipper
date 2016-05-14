#ifndef __fmr_h__
#define __fmr_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Include all target related definitions exposed by the Flipper Message Runtime. */
#include <flipper/fmr/target.h>

/* Include platform specific definitions related to the FMR. */
#include <flipper/platform/platform.h>

#include <flipper/fs.h>

typedef uint32_t fmr_module;

#define fmr_bundle_id_from_string(string) checksum(string, strlen(string))

/* Define macros to abstract the FMR API. */
#define __fmr_count_implicit(_0, _1, _2, _3, _4, _5, _6, n, ...) n
#define __fmr_count(...) __fmr_count_implicit(__VA_ARGS__, 6, 5, 4, 3, 2, 1, 0)
#define __six_args(format, x, ...) format(x), __five_args(format, __VA_ARGS__)
#define __five_args(format, x, ...) format(x), __four_args(format, __VA_ARGS__)
#define __four_args(format, x, ...) format(x), __three_args(format, __VA_ARGS__)
#define __three_args(format, x, ...) format(x), __two_args(format, __VA_ARGS__)
#define __two_args(format, x, ...) format(x), __one_arg(format, __VA_ARGS__)
#define __one_arg(format, x) format(x)
#define __zero_args() NO_ARGS
#define argument_list(size, format, ...) (__fmr_count(0, ##__VA_ARGS__) * size * 2), __fmr_count(0, ##__VA_ARGS__, __six_args, __five_args, __four_args, __three_args, __two_args, __one_arg, __zero_args)(format, __VA_ARGS__)

#if host_argument_size == 2
#define host_arg32(value) value
#else
#define host_arg32(value) hi16(value), lo16(value)
#endif

#if self_argument_size == 2
#define self_arg32(value) value
#else
#define self_arg32(value) hi16(value), lo16(value)
#endif

#define host_args(...) argument_list(host_argument_size, host_argument, __VA_ARGS__)
#define self_args(...) argument_list(self_argument_size, self_argument, __VA_ARGS__)
#define device_args(...) argument_list(device_argument_size, device_argument, __VA_ARGS__)
#define fmr_argument(value) host_argument(value)
#define fmr_args(...) host_args(__VA_ARGS__)

#if device_argument_size == 2
#define device_arg32(value) value
#else
#define device_arg32(value) hi16(value), lo16(value)
#endif

#define NO_ARGS 0

typedef struct _fmr_list {
	uint32_t arg;
	struct _fmr_list *next;
} fmr_list;

/* Declare the virtual interface for this module. */
extern const struct _fmr {
	void (* configure)(void);
	fmr_module (* bind)(char *bundle);
	uint32_t (* invoke)(fmr_module handle, uint8_t index, uint8_t argc, ...);
	uint32_t (* invoke_list)(fmr_module handle, uint8_t index, fmr_list *args);
	void *(* resolve)(void *source, size_t length);
} fmr;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _fmr_configure, _fmr_bind, _fmr_invoke, _fmr_resolve };

/* Declare each prototype for all functions within this driver. */
void fmr_configure(void);
fmr_module fmr_bind(char *bundle);
uint32_t fmr_invoke(fmr_module handle, uint8_t index, uint8_t argc, ...);
uint32_t fmr_invoke_list(fmr_module handle, uint8_t index, fmr_list *args);
void *fmr_resolve(void *source, size_t length);

#endif
#endif
