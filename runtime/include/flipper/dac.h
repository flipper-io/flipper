#ifndef __dac_h__
#define __dac_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _dac {
	int (* configure)(void);
} dac;

#ifdef __private_include__

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _dac;

/* Declare the FMR overlay for this module. */
enum { _dac_configure };

/* Declare the prototypes for all of the functions within this module. */
int dac_configure(void);

#endif
#endif
