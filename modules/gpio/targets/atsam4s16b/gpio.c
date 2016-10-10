#define __private_include__
#include <flipper/gpio.h>
#include <platform/atsam4s16b.h>

int gpio_configure(void) {
	/* Enable the PIOA clock in the PMC. */
	PMC_EnablePeripheral(ID_PIOA);
	gpio_enable(PIO_PA8, PIO_DEFAULT);
	gpio_write(PIO_PA8, 1);
	return 0xdeadbeef;
}

void gpio_enable(uint32_t mask, uint8_t properties) {
	/* Declare a pin map that will configure the appropriate output pins for the the configuration. */
	Pin pins[] = { (Pin){ mask, PIOA, ID_PIOA, PIO_OUTPUT_0, properties } };
	/* Write the pinmap into the PIO. */
	PIO_Configure(pins, PIO_LISTSIZE(pins));
}

void gpio_write(uint32_t mask, uint8_t value) {
	/* Declare a pin map that will configure the appropriate output pins for the the configuration. */
	Pin pin = { mask, PIOA, 0, 0, 0 };
	/* Write the pinmap into the PIO. */
	if (value) {
		PIO_Set(&pin);
	} else {
		PIO_Clear(&pin);
	}
}

uint16_t gpio_read(uint8_t pin) {

	return 0;
}
