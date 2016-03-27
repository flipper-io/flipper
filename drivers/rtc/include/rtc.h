#ifndef __rtc_h__
#define __rtc_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/flipper/core.h>

/* ~ Declare the virtual driver object. ~ */
extern const struct _rtc {

	void (* configure)(void);

} rtc;

#ifdef __private_include__

/* ~ Declare the FMR overlay for this driver object. ~ */
enum { _rtc_configure };

/* ~ Declare all function prototypes for this driver. ~ */
extern void rtc_configure(void);

#endif
#endif
