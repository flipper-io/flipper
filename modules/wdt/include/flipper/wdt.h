#ifndef __wdt_h__
#define __wdt_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _wdt {
	int (* configure)(void);
	void (* fire)(void);
} wdt;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _wdt_configure, _wdt_fire };

/* Declare each prototype for all functions within this driver. */
int wdt_configure(void);
void wdt_fire(void);

#endif
#endif
