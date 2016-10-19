#ifndef __fld_h__
#define __fld_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

#ifdef __private_include__

/* Declare the virtual interface for this module. */
extern const struct _fld {
	void (* configure)(void);
} fld;

/* Declare the FMR overlay for this driver. */
enum { _fld_configure };

/* Declare each prototype for all functions within this driver. */
void fld_configure(void);

#endif
#endif
