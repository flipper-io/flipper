#include <flipper.h>

enum { _led_rgb, _led_configure };

void led_rgb(uint8_t r, uint8_t g, uint8_t b);
int led_configure(void);

void *led_interface[] = {
	&led_rgb,
	&led_configure
};

LF_MODULE(led, "led", led_interface);

LF_WEAK void led_rgb(uint8_t r, uint8_t g, uint8_t b) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "led", _led_rgb, lf_void_t, &retval, lf_args(lf_infer(r), lf_infer(g), lf_infer(b)));
	
}

LF_WEAK int led_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "led", _led_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

