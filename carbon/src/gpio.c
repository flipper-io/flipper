#include <flipper.h>

enum { _gpio_read, _gpio_write, _gpio_enable, _gpio_configure };

uint32_t gpio_read(uint32_t mask);
void gpio_write(uint32_t set, uint32_t clear);
void gpio_enable(uint32_t enable, uint32_t disable);
int gpio_configure(void);

void *gpio_interface[] = {
	&gpio_read,
	&gpio_write,
	&gpio_enable,
	&gpio_configure
};

LF_MODULE(gpio, "gpio", gpio_interface);

LF_WEAK uint32_t gpio_read(uint32_t mask) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "gpio", _gpio_read, lf_int32_t, &retval, lf_args(lf_infer(mask)));
	return (uint32_t)retval;
}

LF_WEAK void gpio_write(uint32_t set, uint32_t clear) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "gpio", _gpio_write, lf_void_t, &retval, lf_args(lf_infer(set), lf_infer(clear)));
	
}

LF_WEAK void gpio_enable(uint32_t enable, uint32_t disable) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "gpio", _gpio_enable, lf_void_t, &retval, lf_args(lf_infer(enable), lf_infer(disable)));
	
}

LF_WEAK int gpio_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_selected(), "gpio", _gpio_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

