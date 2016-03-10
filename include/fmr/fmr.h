#ifndef __fmr_h__
#define __fmr_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* ~ Include all target related definitions exposed by the Flipper Message Runtime. ~ */
#include "target.h"

typedef uint32_t fmr_handle;

#define fmr_bundle_id_from_string(string) checksum(string, strlen(string))
#define fmr_argument(value) little(lo16(value)), little(hi16(value))

/* ~ Define macros to abstract the FMR API. ~ */
#define __fmr_count_implicit(_0, _1, _2, _3, _4, _5, _6, n, ...) n
#define __fmr_count(...) __fmr_count_implicit(__VA_ARGS__, 6, 5, 4, 3, 2, 1, 0)
#define __six_args(x, ...) fmr_argument(x), __five_args(__VA_ARGS__)
#define __five_args(x, ...) fmr_argument(x), __four_args(__VA_ARGS__)
#define __four_args(x, ...) fmr_argument(x), __three_args(__VA_ARGS__)
#define __three_args(x, ...) fmr_argument(x), __two_args(__VA_ARGS__)
#define __two_args(x, ...) fmr_argument(x), __one_arg(__VA_ARGS__)
#define __one_arg(x) fmr_argument(x)
#define __zero_args() NO_ARGS
#define fmr_args(...) (__fmr_count(0, ##__VA_ARGS__) * 2), __fmr_count(0, ##__VA_ARGS__, __six_args, __five_args, __four_args, __three_args, __two_args, __one_arg, __zero_args)(__VA_ARGS__)

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