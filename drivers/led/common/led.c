#define __private_include__
#include <led/led.h>

/* ~ Define the virtual driver object. ~ */
const struct _led led = {

	led_configure,
	led_set_rgb

};