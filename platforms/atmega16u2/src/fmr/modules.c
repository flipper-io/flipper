#define __private_include__
#include <flipper/fmr.h>
#include <flipper/modules.h>
#include <platform/atmega16u2.h>

/* Define the standard modules accessible on this platform. */
const void *const fmr_modules[] PROGMEM = {
    _forward_id,    // adc
    &button,        // button
    &cpu,           // cpu
    _forward_id,    // dac
    _forward_id,    // error
    &fmr,           // fmr
    _forward_id,    // fs
    _forward_id,    // gpio
    _forward_id,    // i2c
    &led,           // led
    _forward_id,    // pwm
    _forward_id,    // rtc
    _forward_id,    // spi
    _forward_id,    // swd
    _forward_id,    // temp
    _forward_id,    // timer
    &uart,           // uart
    _forward_id,    // usb
    _forward_id,    // wdt
};

const void *lf_std_function(fmr_module module, fmr_function function) {
    /* Dereference the pointer to the target module. */
    void *object = (void *)(pgm_read_word(&fmr_modules[module]));
    /* Dereference and return a pointer to the target function. */
    return ((void **)(object))[function];
}
