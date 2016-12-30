#define __private_include__
#include <flipper/flipper.h>
#include <flipper/carbon/wdt.h>

int wdt_configure(void) {
	return lf_invoke(&_wdt, _wdt_configure, NULL);
}

void wdt_fire(void) {
	lf_invoke(&_wdt, _wdt_fire, NULL);
}
