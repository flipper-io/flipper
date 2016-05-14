#ifndef __timer_h__
#define __timer_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _timer {

	void (* configure)(void);

} timer;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _timer_configure };

/* Declare each prototype for all functions within this driver. */
void timer_configure(void);

#endif
#endif
