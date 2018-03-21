#include <flipper.h>

extern struct _lf_module wdt;

LF_FUNC("wdt") int wdt_configure(void) {
	dyld_register(&THIS_DEVICE, &wdt);
	printf("Configured the watchdog.\n");
	return 0;
}

LF_FUNC("wdt") void wdt_fire(void) {
	printf("Firing the watchdog.\n");
}
