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
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "wdt", _wdt_fire, lf_void_t, &retval, NULL);
	
}

LF_WEAK int wdt_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "wdt", _wdt_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

