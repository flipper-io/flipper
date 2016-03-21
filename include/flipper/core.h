#ifndef __core_h__
#define __core_h__

/* ~ Include the standard library headers. ~ */
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ~ Include types exposed by the Flipper Message Runtime. ~ */
#include <fmr/bus.h>

/* ~ Define the filesystem pointer type. ~ */
typedef uint32_t fsp;

/* ~ Define a macro for verbose printing. ~ */
#if defined(__verbose__)
#define verbose(...) fprintf(stdout, __VA_ARGS__);
#else
#define verbose(...) ;
#endif

#define ceiling(x, y) ((x + y - 1) / y)

#define HIGH    true
#define LOW     false

#define pull_up true

#define INPUT   false
#define OUTPUT  true

/* ~ Define bit manipulation macros. ~ */
#define bit(b)                                          (0x01 << (b))
#define get_bit_from_port(b, p)                         ((p) & bit(b))
#define set_bit_in_port(b, p)                           ((p) |= bit(b))
#define set_bits_in_port_with_mask(p, m)                ((p) |= (m))
#define clear_bit_in_port(b, p)                         ((p) &= ~(bit(b)))
#define clear_bits_in_port_with_mask(p, m)              ((p) &= ~(m))
#define flip_bit_in_port(b, p)                          ((p) ^= bit(b))
#define flip_bits_in_port_with_mask(p, m)               ((p) ^= (m))

#define lo(x) ((uint8_t)(x))
#define hi(x) ((uint8_t)(x >> 8))

#define lo16(x) ((uint16_t)(((uint32_t)(x))))
#define hi16(x) ((uint16_t)(((uint32_t)(x)) >> 16))

#define little(x)	((((uint16_t)(x)) << 8 ) | (((uint16_t)(x)) >> 8))
#define little32(x) ((((uint32_t)(x)) << 16 ) | (((uint32_t)(x)) >> 16))

/* ~ These macros are defined in platform specific header files. ~ */
#define enable_interrupts()
#define disable_interrupts()

#define CARBON_VENDOR_ID        0x16C0
#define CARBON_PRODUCT_ID       0x0480
#define CARBON_USAGE_PAGE       0xFFAB
#define CARBON_USAGE            0x0200

#endif
