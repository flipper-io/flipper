#ifndef __qux_h__
#define __qux_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/libflipper.h>

/* Declare the virtual interface for this module. */
extern const struct _qux {
	int (* configure)(void);
    void (* test)(void);
} qux;

#ifdef __private_include__

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _qux;

/* Declare the FMR overlay for this module. */
enum { _qux_configure, _qux_test };

/* Declare the prototypes for all of the functions within this module. */
int qux_configure(void);
void qux_test(void);

#endif
#endif
