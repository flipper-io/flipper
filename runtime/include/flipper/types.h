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

/* Terminal colors. */
#ifndef __DISABLE_TERMINAL_COLORS__
#define KNRM "\x1B[0m"
#define KGRN "\x1B[32m"
#define KRED "\x1B[31m"
#define KBLU "\x1B[34m"
#define KYEL "\x1B[33m"
#else
#define KNRM ""
#define KGRN ""
#define KRED ""
#define KBLU ""
#define KYEL ""
#endif

/* Packed attribute. */
#define LF_PACKED __attribute__((__packed__))
/* Weak attribute. */
#define LF_WEAK __attribute__((weak))

#ifdef __clang__
#define LF_FUNC(MODULE) __attribute__((section("__TEXT,.lf.module."MODULE), used))
#else
#define LF_FUNC(MODULE) __attribute__((section(".lf.module."MODULE), used))
#endif

/* Used to contain the result of checksumming operations. */
typedef uint16_t lf_crc_t;
/* Used to quantify block sizes sent accross different platforms. */
typedef uint32_t lf_size_t;
/* Used to quantify the version of modules in a standardized format. */
typedef uint16_t lf_version_t;
/* Used to specify return types. */
typedef uint32_t lf_return_t;
/* Describes a type used to contain libflipper error codes. */
typedef uint8_t lf_error_t;

#endif
