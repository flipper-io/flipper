#define __private_include__
#include <flipper/io/io.h>

#include <platform.h>

void io_configure(void) {

	/* ~ Enable the PIOA (Peripheral IO Controller A) in the PCER (Peripheral Clock Enable Register). ~ */
	set_bit_in_port(AT91C_ID_PIOA, AT91C_BASE_PMC -> PMC_PCER);

}

void io_set_direction(uint8_t pin, uint8_t direction) {

	/* ~ If the pin is less than or equal to IO16, the pin is digital. ~ */
	if (pin <= IO16)

		digital_set_direction(pin, direction);

	/* ~ If not, the pin is analog. ~ */
	else

		analog_set_direction(pin, direction);

}

void io_write(uint8_t pin, uint16_t value) {

	/* ~ If the pin is less than or equal to IO16, the pin is digital. ~ */
	if (pin <= IO16)

		digital_write(pin, value);

	/* ~ If not, the pin is analog. ~ */
	else

		analog_write(pin, value);

}

uint16_t io_read(uint8_t pin) {

	/* ~ If the pin is less than or equal to IO16, the pin is digital. ~ */
	if (pin <= IO16)

		return digital_read(pin);

	/* ~ If not, the pin is analog. ~ */
	else

		return analog_read(pin);

}
