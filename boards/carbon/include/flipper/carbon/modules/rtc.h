#ifndef __rtc_h__
#define __rtc_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/libflipper.h>

/* Declare the virtual interface for this module. */
extern const struct _rtc {
	int (* configure)(void);
} rtc;

#ifdef __private_include__

/* The fmr_module structure for this module. */
extern struct _lf_module _rtc;

/* Declare the FMR overlay for this driver. */
enum { _rtc_configure };

/* Declare each prototype for all functions within this driver. */
int rtc_configure(void);

#endif
#endif
