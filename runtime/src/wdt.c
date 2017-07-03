#include <flipper/libflipper.h>

#ifdef __use_wdt__
#define __private_include__
#include <flipper/wdt.h>

LF_MODULE(_wdt, "wdt", "Handles interaction with the internal watchdog timer.", _wdt_id);

/* Define the virtual interface for this module. */
const struct _wdt wdt = {
	wdt_configure,
	wdt_fire
};

LF_WEAK int wdt_configure(void) {
	return lf_invoke(&_wdt, _wdt_configure, NULL);
}

LF_WEAK void wdt_fire(void) {
	lf_invoke(&_wdt, _wdt_fire, NULL);
}

#endif
