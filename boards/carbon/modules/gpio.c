#define __private_include__
#include <flipper/carbon/modules/gpio.h>

#ifdef __use_gpio__
/* Define the virtual interface for this module. */
const struct _gpio gpio = {
	gpio_configure,
	gpio_enable,
	gpio_write,
	gpio_read
};
#endif
