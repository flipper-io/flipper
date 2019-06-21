#include "libflipper.h"

LF_FUNC int wdt_configure(void) {
    printf("Configured the watchdog.\n");
    return 0;
}

LF_FUNC void wdt_fire(void) {
    printf("Firing the watchdog.\n");
}
