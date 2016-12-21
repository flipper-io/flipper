#define __private_include__
#include <flipper/rtc.h>

#ifdef __use_rtc__
/* Define the virtual interface for this module. */
const struct _rtc rtc = {
	rtc_configure
};
#endif
