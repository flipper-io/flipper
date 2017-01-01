/* core.h - Exposes all types and macros provided by the Flipper Toolbox. */

#ifndef __lf_core_h__
#define __lf_core_h__

/* Include the standard library headers. */
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Packed attribute. */
#define LF_PACKED __attribute__((__packed__))
/* Weak attribute. */
#define LF_WEAK __attribute__((weak))

/* Used to contain the result of checksumming operations. */
typedef uint16_t lf_crc_t;

/* Describes a pointer to an address within non-volatile memory. */
typedef uint32_t nvm_p;

/* Used to quantify block sizes sent accross different platforms. */
typedef uint32_t lf_size_t;

/* Used to quantify the version of modules in a standardized format. */
typedef uint16_t lf_version_t;

#endif
