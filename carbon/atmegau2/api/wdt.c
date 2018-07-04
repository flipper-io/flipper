#include <flipper.h>
#include "wdt.h"
#include "atmegau2.h"

LF_FUNC("wdt") int wdt_configure(void) {
	return lf_success;
}

LF_FUNC("wdt") void wdt_fire(void) {
	/* Enable the watchdog timer. */
	wdt_enable(WDTO_15MS);
}
