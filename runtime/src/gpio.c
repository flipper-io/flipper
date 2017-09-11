#include <flipper/libflipper.h>

#ifdef __use_gpio__
#define __private_include__
#include <flipper/gpio.h>

LF_MODULE(_gpio, "gpio", "Interfaces with the device's general purpose input output pins.");

/* Define the virtual interface for this module. */
const struct _gpio gpio = {
	gpio_configure,
	gpio_enable,
	gpio_write,
	gpio_read
};

LF_WEAK int gpio_configure(void) {
	return lf_invoke(&_gpio, _gpio_configure, NULL);
}

LF_WEAK void gpio_enable(uint32_t enable, uint32_t disable) {
	lf_invoke(&_gpio, _gpio_enable, fmr_args(fmr_infer(enable), fmr_infer(disable)));
}

LF_WEAK void gpio_write(uint32_t set, uint32_t clear) {
	lf_invoke(&_gpio, _gpio_write, fmr_args(fmr_infer(set), fmr_infer(clear)));
}

LF_WEAK uint32_t gpio_read(uint32_t mask) {
	return lf_invoke(&_gpio, _gpio_read, fmr_args(fmr_infer(mask)));
}

#endif
