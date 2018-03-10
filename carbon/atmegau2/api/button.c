#include <flipper/button.h>

LF_FUNC("button") int button_configure(void) {
	BUTTON_DDR &= ~(1 << BUTTON_PIN);
	return lf_success;
}

LF_FUNC("button") uint8_t button_read(void) {
	return ((BUTTON_IN & (1 << BUTTON_PIN)) >> BUTTON_PIN);
}
