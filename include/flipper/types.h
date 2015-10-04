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

#include <fmr/bus.h>

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

#define bit(b)                              (0x01 << (b))

#define get_bit_from_port(b, p)             ((p) & bit(b))

#define set_bit_in_port(b, p)               ((p) |= bit(b))

#define set_bits_in_port_with_mask(p, m)	((p) |= (m))

#define clear_bit_in_port(b, p)             ((p) &= ~(bit(b)))

#define clear_bits_in_port_with_mask(p, m)	((p) &= ~(m))

#define flip_bit_in_port(b, p)              ((p) ^= bit(b))

#define flip_bits_in_port_with_mask(p, m)	((p) ^= (m))

#define lo(x)	((uint8_t)(x))

#define hi(x)	((uint8_t)(x >> 8))

#define lo16(x)	((uint16_t)(((uint32_t)(x))))

#define hi16(x)	((uint16_t)(((uint32_t)(x)) >> 16))

#define little(x) ((((uint16_t)(x)) << 8 ) | (((uint16_t)(x)) >> 8))

/* ~ These macros are defined in platform specific header files. ~ */

#define enable_interrupts()

#define disable_interrupts()

/* ~ Miscelaneous. ~ */

#define NO_ARGS 0

#endif