#include <flipper.h>

LF_FUNC("wdt") int wdt_configure(void) {
	printf("Configured the watchdog.\n");
	return 0;
}

LF_FUNC("wdt") void wdt_fire(void) {
	printf("Firing the watchdog.\n");
}
