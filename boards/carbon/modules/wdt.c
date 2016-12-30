#define __private_include__
#include <flipper/carbon/modules/wdt.h>

#ifdef __use_wdt__
/* Define the virtual interface for this module. */
const struct _wdt wdt = {
	wdt_configure,
	wdt_fire
};
#endif
