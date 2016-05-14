#ifndef __temp_h__
#define __temp_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _temp {

	void (* configure)(void);

} temp;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _temp_configure };

/* Declare each prototype for all functions within this driver. */
void temp_configure(void);

#endif
#endif
