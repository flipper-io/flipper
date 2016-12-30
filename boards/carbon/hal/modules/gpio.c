#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/gpio.h>

int gpio_configure(void) {
	return lf_invoke(&_gpio, _gpio_configure, NULL);
}

void gpio_enable(uint32_t enable, uint32_t disable) {
	lf_invoke(&_gpio, _gpio_enable, fmr_args(fmr_infer(enable), fmr_infer(disable)));
}

void gpio_write(uint32_t set, uint32_t clear) {
	lf_invoke(&_gpio, _gpio_write, fmr_args(fmr_infer(set), fmr_infer(clear)));
}

uint32_t gpio_read(uint32_t mask) {
	return lf_invoke(&_gpio, _gpio_read, fmr_args(fmr_infer(mask)));
}
