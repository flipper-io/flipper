#ifndef __temp_h__
#define __temp_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _temp {
	int (* configure)(void);
} temp;

#ifdef __private_include__

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _temp;

/* Declare the FMR overlay for this module. */
enum { _temp_configure };

/* Declare the prototypes for all of the functions within this module. */
int temp_configure(void);

#endif
#endif
