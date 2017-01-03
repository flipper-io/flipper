#define __private_include__
#include <flipper/carbon/platforms/atsam4s16b/modules.h>

/* Define the standard modules based on platform specific usage declarations. */
const void *const fmr_modules[] = {
    &adc,
    &button,
    &dac,
    &fld,
    &fs,
    &gpio,
    &i2c,
    &led,
    &pwm,
    &rtc,
    &spi,
    &swd,
    &temp,
    &timer,
    &usart,
    &usb,
    &wdt
};
