#include <flipper/flipper.h>

int wdt_configure(void) {
    printf("Configured the watchdog.\n");
    return 0;
}

void wdt_fire(void) {
    printf("Firing the watchdog.\n");
}
