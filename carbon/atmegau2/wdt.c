#include <flipper/wdt.h>


int wdt_configure(void) {
	return lf_success;
}

void wdt_fire(void) {
	/* Enable the watchdog timer. */
	wdt_enable(WDTO_15MS);
}
