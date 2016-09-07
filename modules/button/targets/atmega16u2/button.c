#define __private_include__
#include <flipper/button.h>
#include <platform/atmega16u2.h>

void button_configure(void) {
	/* Enable the button as an input. */
	clear_bit_in_port(BUTTON_PIN, BUTTON_DDR);
	/* Enable the reset button interrupt. */
	//PCMSK1 |=  (1 << PCINT8);
	//PCICR  |=  (1 << PCIE1);
}

uint8_t button_read(void) {
	return (BUTTON_IN & 1);
}
