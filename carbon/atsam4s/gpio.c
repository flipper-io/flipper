#define __private_include__
#include <flipper/gpio.h>
#include <flipper/atsam4s/atsam4s.h>

int gpio_configure(void) {
	/* Enable the PIOA clock in the PMC. */
	PMC -> PMC_PCER0 = (1 << ID_PIOA);
	return lf_success;
}

void gpio_enable(uint32_t enable, uint32_t disable) {
	printf("gpio e:0x%08x d:0x%08x\n", enable, disable);
	PIOA -> PIO_PER |= enable;
	PIOA -> PIO_OER |= enable;
	PIOA -> PIO_ODR |= disable;
}

void gpio_write(uint32_t set, uint32_t clear) {
	printf("gpio s:0x%08x c:0x%08x\n", set, clear);
	PIOA -> PIO_SODR |= set;
	PIOA -> PIO_CODR |= clear;
}

uint32_t gpio_read(uint32_t mask) {
	return PIOA -> PIO_PDSR;
}