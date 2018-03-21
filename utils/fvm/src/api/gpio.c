#include <flipper.h>

extern struct _lf_module gpio;

int gpio_configure(void) {
	dyld_register(&THIS_DEVICE, &gpio);
	printf("Configured gpio controller.\n");
	return lf_success;
}

void gpio_enable(uint32_t enable, uint32_t disable) {
	printf("Enabling gpio pins 0x%08x, disabling gpio pins 0x%08x.\n", enable, disable);
}

void gpio_write(uint32_t set, uint32_t clear) {
	printf("Setting gpio pins 0x%08x, clearing gpio pins 0x%08x.\n", set, clear);
}

uint32_t gpio_read(uint32_t mask) {
	printf("Reading gpio pins with mask 0x%08x.\n", mask);
	return lf_success;
}
