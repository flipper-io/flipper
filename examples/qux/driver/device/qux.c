#include <qux.h>

#include "atmel.h"

#define PIN 7

void qux_configure(void) {
	
	AT91C_BASE_PIOA -> PIO_PER |= (1 << PIN);
	AT91C_BASE_PIOA -> PIO_OER |= (1 << PIN);
	
}

void qux_on(void) {
	
	AT91C_BASE_PIOA -> PIO_SODR |= (1 << PIN);
	
}

void qux_off(void) {
	
	AT91C_BASE_PIOA -> PIO_CODR |= (1 << PIN);
	
}