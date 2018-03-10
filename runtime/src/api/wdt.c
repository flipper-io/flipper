#include <flipper/wdt.h>

#ifdef __use_wdt__

LF_MODULE(_wdt, "wdt", "Handles interaction with the internal watchdog timer.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _wdt_interface wdt = {
	wdt_configure,
	wdt_fire
};

LF_WEAK int wdt_configure(void) {
	return lf_invoke(lf_get_current_device(), &_wdt, _wdt_configure, lf_int_t, NULL);
}

LF_WEAK void wdt_fire(void) {
	lf_invoke(lf_get_current_device(), &_wdt, _wdt_fire, lf_int_t, NULL);
}

#endif
