#define __private_include__
#include <flipper/button.h>
#include <flipper/atmegau2/atmegau2.h>

int button_configure(void) {
	/* Enable the button as an input. */
	clear_bit_in_port(BUTTON_PIN, BUTTON_DDR);
	return lf_success;
}

uint8_t button_read(void) {
	return ((BUTTON_IN & (1 << BUTTON_PIN)) >> BUTTON_PIN);
}
