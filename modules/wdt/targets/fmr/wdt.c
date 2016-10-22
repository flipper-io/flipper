#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_wdt, "wdt", "Handles interaction with the internal watchdog timer.", _wdt_id);

int wdt_configure(void) {
	return lf_invoke(&_wdt, _wdt_configure, NULL);
}

void wdt_fire(void) {
	lf_invoke(&_wdt, _wdt_fire, NULL);
}
