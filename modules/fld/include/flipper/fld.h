#ifndef __fld_h__
#define __fld_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

#ifdef __private_include__

/* Declare the virtual interface for this module. */
extern const struct _fld {
	int (* configure)(void);
	/* Loads a module given its identifier. */
	int (* load)(lf_id_t identifier);
} fld;

/* Declare the FMR overlay for this driver. */
enum { _fld_configure, _fld_load };

/* Declare each prototype for all functions within this driver. */
int fld_configure(void);
int fld_load(lf_id_t identifier);

#endif
#endif
