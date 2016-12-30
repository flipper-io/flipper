#ifndef __dac_h__
#define __dac_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _dac {
	int (* configure)(void);
} dac;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _dac_configure };

/* Declare each prototype for all functions within this driver. */
int dac_configure(void);

#endif
#endif
