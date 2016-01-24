#define __private_include__

#include <button/button.h>

#include <platform/atmega.h>

void button_configure(void) {
	
	DDRD  |= (INPUT << BUTTON_PIN);
	
	/* ~ Enable the power button interrupt. ~ */
	
	EIMSK |= (1 << INT0); EICRA |= ((1 << ISC01) | (1 << ISC00));
	
}

bool button_read(void) {
	
	return (PIND & 1);
	
}