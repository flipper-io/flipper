#include <flipper/gpio.h>

#ifdef __use_gpio__

LF_MODULE(_gpio, "gpio", "Interfaces with the device's general purpose input output pins.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _gpio_interface gpio = {
	gpio_configure,
	gpio_enable,
	gpio_write,
	gpio_read
};

LF_WEAK int gpio_configure(void) {
	return lf_invoke(lf_get_current_device(), &_gpio, _gpio_configure, lf_int_t, NULL);
}

LF_WEAK void gpio_enable(uint32_t enable, uint32_t disable) {
	lf_invoke(lf_get_current_device(), &_gpio, _gpio_enable, lf_void_t, lf_args(lf_infer(enable), lf_infer(disable)));
}

LF_WEAK void gpio_write(uint32_t set, uint32_t clear) {
	lf_invoke(lf_get_current_device(), &_gpio, _gpio_write, lf_void_t, lf_args(lf_infer(set), lf_infer(clear)));
}

LF_WEAK uint32_t gpio_read(uint32_t mask) {
	return lf_invoke(lf_get_current_device(), &_gpio, _gpio_read, lf_int32_t, lf_args(lf_infer(mask)));
}

#endif
