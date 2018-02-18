#ifndef __rtc_h__
#define __rtc_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper.h>

/* Declare the virtual interface for this module. */
extern const struct _rtc_interface {
	int (* configure)(void);
} rtc;

#ifdef __private_include__

/* Declare the _lf_module structure for this module. */
extern struct _lf_module _rtc;

/* Declare the FMR overlay for this module. */
enum { _rtc_configure };

/* Declare the prototypes for all of the functions within this module. */
int rtc_configure(void);

#endif
#endif
