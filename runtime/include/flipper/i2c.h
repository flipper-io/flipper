#ifndef __i2c_h__
#define __i2c_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _i2c {
	int (* configure)(void);
} i2c;

#ifdef __private_include__

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _i2c;

/* Declare the FMR overlay for this module. */
enum { _i2c_configure };

/* Declare the prototypes for all of the functions within this module. */
int i2c_configure(void);

#endif
#endif
