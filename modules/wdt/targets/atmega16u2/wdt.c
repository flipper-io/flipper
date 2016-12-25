#define __private_include__
#include <flipper/wdt.h>
#include <platforms/atmega16u2.h>

int wdt_configure(void) {
	return lf_success;
}

void wdt_fire(void) {
	/* Enable the watchdog timer. */
	wdt_enable(WDTO_15MS);
}
