#include <flipper.h>

enum { _wdt_fire, _wdt_configure };

void wdt_fire(void);
int wdt_configure(void);

void *wdt_interface[] = {
	&wdt_fire,
	&wdt_configure
};

LF_MODULE(wdt, "wdt", wdt_interface);

LF_WEAK void wdt_fire(void) {
	lf_invoke(lf_get_current_device(), "wdt", _wdt_fire, lf_void_t, NULL);
}

LF_WEAK int wdt_configure(void) {
	return lf_invoke(lf_get_current_device(), "wdt", _wdt_configure, lf_int32_t, NULL);
}

