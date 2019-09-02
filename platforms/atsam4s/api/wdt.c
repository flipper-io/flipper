#include "libflipper.h"
#include <wdt.h>

LF_FUNC int wdt_configure(void) {
    return lf_success;
}

LF_FUNC void wdt_fire(void) {
}
