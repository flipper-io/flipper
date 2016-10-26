#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_gpio, "gpio", "Interfaces with the device's general purpose input output pins.", _gpio_id);

int gpio_configure(void) {
	return lf_invoke(&_gpio, _gpio_configure, NULL);
}

void gpio_enable(uint32_t mask, uint8_t properties) {
	lf_invoke(&_gpio, _gpio_enable, fmr_args(fmr_infer(mask), fmr_infer(properties)));
}

void gpio_write(uint32_t mask, uint8_t value) {
	lf_invoke(&_gpio, _gpio_write, fmr_args(fmr_infer(mask), fmr_infer(value)));
}

uint16_t gpio_read(uint8_t pin) {
	return lf_invoke(&_gpio, _gpio_read, fmr_args(fmr_infer(pin)));
}
