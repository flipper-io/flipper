#include <flipper.h>

#ifdef __use_wdt__
#define __private_include__
#include <flipper/wdt.h>

int wdt_configure(void) {
	printf("Configuring the watchdog.\n");
	return 0;
}

void wdt_fire(void) {
	printf("Firing the watchdog.\n");
}

#endif
