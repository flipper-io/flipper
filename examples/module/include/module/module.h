#ifndef __module_h__
#define __module_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/libflipper.h>

/* Declare the virtual interface for this module. */
extern const struct _module {
	int (* configure)(void);
    void (* test)(void);
} module;

#ifdef __private_include__

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _module;

/* Declare the FMR overlay for this module. */
enum { _module_configure, _module_test };

/* Declare the prototypes for all of the functions within this module. */
int module_configure(void);
void module_test(void);

#endif
#endif
