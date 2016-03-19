#ifndef __crc_h__
#define __crc_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

#ifdef __private_include__

/* ~ Declare all function prototypes. ~ */
extern uint16_t checksum(void *source, uint16_t length);

#endif
#endif