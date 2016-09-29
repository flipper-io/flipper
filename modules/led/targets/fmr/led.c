#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
struct _fmr_module _led = {
    "led",
    "Controls the on-board RGB status LED.",
    LF_VERSION,
    _led_id,
    NULL
};

void led_configure(void) {
    lf_invoke(&_led, _led_configure, NULL);
}

void led_set_rgb(uint8_t r, uint8_t g, uint8_t b) {
    lf_invoke(&_led, _led_set_rgb, fmr_args(fmr_int8(r), fmr_int8(g), fmr_int8(b)));
}
