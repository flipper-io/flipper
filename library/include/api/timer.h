#ifndef __timer_h__
#define __timer_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the prototypes for all of the functions within this module. */
int timer_configure(void);
int timer_register(uint32_t ticks, void *callback);

#endif
