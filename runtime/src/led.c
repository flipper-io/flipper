#define __private_include__
#include <flipper/led.h>

#ifdef __use_led__

LF_MODULE(_led, "led", "Interacts with the built-in status LED.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _led led = {
	led_configure,
	led_rgb
};

LF_WEAK int led_configure(void) {
	lf_invoke(&_led, _led_configure, fmr_int_t, NULL);
	return lf_success;
}

LF_WEAK void led_rgb(uint8_t r, uint8_t g, uint8_t b) {
	lf_invoke(&_led, _led_rgb, fmr_void_t, fmr_args(fmr_infer(r), fmr_infer(g), fmr_infer(b)));
}

#endif
