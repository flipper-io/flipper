#define __private_include__
#include <flipper/button.h>
#include <flipper/platform/platform.h>

void button_configure(void) {

	/* ~ Enable the button as an input. ~ */
	DDRD |= (INPUT << BUTTON_PIN);

	/* ~ Enable the power button interrupt. ~ */
//	EIMSK |= (1 << INT0);
//	EICRA |= (1 << ISC01) | (1 << ISC00);

}

uint8_t button_read(void) {

	return (PIND & 1);

}
