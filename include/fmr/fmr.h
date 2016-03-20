#ifndef __fmr_h__
#define __fmr_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Include all target related definitions exposed by the Flipper Message Runtime. ~ */
#include "target.h"

/* ~ Include platform specific definitions related to the FMR. ~ */
#include <platform.h>

typedef uint32_t fmr_handle;

#define fmr_bundle_id_from_string(string) checksum(string, strlen(string))

/* ~ Define macros to abstract the FMR API. ~ */
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

/* ~ Declare the virtual driver object. ~ */
extern const struct _fmr {

	void (* configure)(void);
	fmr_handle (* bind)(uint16_t bundle);
	uint32_t (* invoke)(fmr_handle handle, uint8_t index, uint8_t argc, ...);
	void *(* resolve)(void *source, uint32_t length);

} fmr;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _fmr_configure, _fmr_bind, _fmr_invoke, _fmr_resolve };

/* ~ Declare all function prototypes for this driver. ~ */
extern void fmr_configure(void);
extern fmr_handle fmr_bind(uint16_t bundle);
extern uint32_t fmr_invoke(fmr_handle handle, uint8_t index, uint8_t argc, ...);
extern void *fmr_resolve(void *source, uint32_t length);

#endif
#endif
