#ifndef __rtc_h__
#define __rtc_h__

/* Include all types and macros exposed by the Flipper Toolbox. */
#include <flipper/core.h>

/* Declare the virtual interface for this module. */
extern const struct _rtc {

	void (* configure)(void);

} rtc;

#ifdef __private_include__

/* Declare the FMR overlay for this driver. */
enum { _rtc_configure };

/* Declare each prototype for all functions within this driver. */
void rtc_configure(void);

#endif
#endif
