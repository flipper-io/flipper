#define __private_include__
#include <flipper/wdt.h>

/* Define the virtual interface for this module. */
const struct _wdt wdt = {
	wdt_configure
};
