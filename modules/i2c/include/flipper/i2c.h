#ifndef __i2c_h__
#define __i2c_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _i2c {
	void (* configure)(void);
} i2c;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _i2c_configure };

/* Declare each prototype for all functions within this driver. */
void i2c_configure(void);

#endif
#endif
