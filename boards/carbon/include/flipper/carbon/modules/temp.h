#ifndef __temp_h__
#define __temp_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/libflipper.h>

/* Declare the virtual interface for this module. */
extern const struct _temp {
	int (* configure)(void);
} temp;

#ifdef __private_include__

/* The fmr_module structure for this module. */
extern struct _lf_module _temp;

/* Declare the FMR overlay for this driver. */
enum { _temp_configure };

/* Declare each prototype for all functions within this driver. */
int temp_configure(void);

#endif
#endif
