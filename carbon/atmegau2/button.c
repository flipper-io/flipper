#define __private_include__
#include <flipper/button.h>
#include <flipper/atmegau2/atmegau2.h>

int button_configure(void) {
	BUTTON_DDR &= ~(1 << BUTTON_PIN);
	return lf_success;
}

uint8_t button_read(void) {
	return ((BUTTON_IN & (1 << BUTTON_PIN)) >> BUTTON_PIN);
}
