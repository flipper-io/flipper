#define __private_include__
#include <flipper/carbon.h>

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
    &task,
    &temp,
    &timer,
    &usart,
    &usb,
    &wdt
};
