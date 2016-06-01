#ifndef __crc_h__
#define __crc_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

typedef uint16_t uintcrc_t;

#ifdef __private_include__

/* Declare all function prototypes. */
uint16_t checksum(void *source, fmr_size_t length);

#endif
#endif
