#define __private_include__
#include <flipper/carbon/modules/led.h>

#ifdef __use_led__
/* Define the virtual interface for this module. */
const struct _led led = {
	led_configure,
	led_rgb
};
#endif
