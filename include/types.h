#ifndef types_h

#define types_h

/* Include the standard library headers. */

#include <stdarg.h>

#include <stdbool.h>

#include <stddef.h>

#include <stdint.h>

#include <stdio.h>

#include <stdlib.h>

#include <string.h>

/* Define the Flipper filesystem pointer type. */

typedef uint32_t fsp;

/* Define a macro for verbose printing. */

#if defined(__verbose__)

#define verbose(...) fprintf(stdout, __VA_ARGS__);

#define error(...) fprintf(stdout, __VA_ARGS__);

#else

#define verbose(...) ;

#define error(...) ;

#endif

/* ~ Bit manipulation, cause let's face it, nobody remembers these. ~ */

#define lo(x)	((uint8_t)(x))

#define hi(x)	((uint8_t)(x >> 8))

#define lo16(x)	((uint16_t)(((uint32_t)(x))))

#define hi16(x)	((uint16_t)(((uint32_t)(x)) >> 16))

#endif