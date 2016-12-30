#define __private_include__
#include <flipper/carbon/platforms/atmega16u2/modules.h>

/* Define the standard module array for this platform. */
const void *const fmr_modules[] = {
    &button,
    &cpu,
    &fmr,
    &fs,
    &led,
    &uart0,
    &wdt
};
