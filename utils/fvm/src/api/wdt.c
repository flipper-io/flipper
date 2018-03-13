#include <flipper.h>

int wdt_configure(void) {
	printf("Configuring the watchdog.\n");
	return 0;
}

void wdt_fire(void) {
	printf("Firing the watchdog.\n");
}
