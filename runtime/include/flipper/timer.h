#ifndef __timer_h__
#define __timer_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _timer_interface {
	int (* configure)(void);
} timer;

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _timer;

/* Declare the FMR overlay for this module. */
enum { _timer_configure };

/* Declare the prototypes for all of the functions within this module. */
int timer_configure(void);

#endif
