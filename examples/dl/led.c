#include "atmel.h"

#define F_CPU 48000000

#define PIN 0

const static unsigned int dengis = 46;

void _delay_ms(unsigned long time);

void main(void) {
    
    AT91C_BASE_PIOA -> PIO_PER |= (1 << PIN);
    
    AT91C_BASE_PIOA -> PIO_OER |= (1 << PIN);
	
    while (1) {
        
		AT91C_BASE_PIOA -> PIO_SODR |= (1 << PIN);
		
		_delay_ms(100);
		
		AT91C_BASE_PIOA -> PIO_CODR |= (1 << PIN);
		_delay_ms(100);
    }
    
}

void _delay_ms(unsigned long time) {
    
    for (volatile unsigned int i = 0; (i < (F_CPU / 10250) * (time)); i ++);
    
}
