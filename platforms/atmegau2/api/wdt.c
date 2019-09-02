#include "libflipper.h"
#include <atmegau2.h>

LF_FUNC int wdt_configure(void) {
    return lf_success;
}

LF_FUNC void wdt_fire(void) {
    /* Enable the watchdog timer. */
    wdt_enable(WDTO_15MS);
}
