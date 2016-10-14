#define __private_include__
#include <flipper/gpio.h>
#include <platform/atsam4s16b.h>

int gpio_configure(void) {
	/* Enable the PIOA clock in the PMC. */
	PMC_EnablePeripheral(ID_PIOA);
	return lf_success;
}

void gpio_enable(uint32_t mask, uint8_t properties) {
	/* Print the parameters to the function for debugging. */
	usart_push(&mask, 4);
	usart_push(&properties, 4);
	usart_put('\n');
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
