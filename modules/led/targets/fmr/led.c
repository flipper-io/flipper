#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_led, "led", "Interacts with the built-in status LED.", _led_id);

int led_configure(void) {
	lf_invoke(&_led, _led_configure, NULL);
	return lf_success;
}

void led_set_rgb(uint8_t r, uint8_t g, uint8_t b) {
	lf_invoke(&_led, _led_set_rgb, fmr_args(fmr_infer(r), fmr_infer(g), fmr_infer(b)));
}
