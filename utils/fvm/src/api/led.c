#include <flipper.h>

extern struct _lf_module led;

LF_FUNC("led") int led_configure(void) {
	dyld_register(&THIS_DEVICE, &led);
	printf("Configured the led.\n");
	return lf_success;
}

LF_FUNC("led") void led_rgb(uint8_t r, uint8_t g, uint8_t b) {
	printf("Setting the led color to R: %i, G: %i, B: %i.\n", r, g, b);
}
