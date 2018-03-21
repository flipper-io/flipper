#ifndef __wdt_h__
#define __wdt_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the prototypes for all of the functions within this module. */
int wdt_configure(void);
void wdt_fire(void);

#endif
