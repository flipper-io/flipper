#include <flipper/led.h>

#ifdef __use_led__

LF_MODULE(_led, "led", "Interacts with the built-in status LED.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _led_interface led = {
	led_configure,
	led_rgb
};

LF_WEAK int led_configure(void) {
	lf_invoke(lf_get_current_device(), &_led, _led_configure, lf_int_t, NULL);
	return lf_success;
}

LF_WEAK void led_rgb(uint8_t r, uint8_t g, uint8_t b) {
	lf_invoke(lf_get_current_device(), &_led, _led_rgb, lf_void_t, lf_args(lf_infer(r), lf_infer(g), lf_infer(b)));
}

#endif
