#include "atmel.h"

#define F_CPU 48000000

void _delay_ms(unsigned long time);

void main(void) {
    
    AT91C_BASE_PIOA -> PIO_PER |= (1 << 7);
    
    AT91C_BASE_PIOA -> PIO_OER |= (1 << 7);
    
    while (1) {
        
        AT91C_BASE_PIOA -> PIO_SODR |= (1 << 7);
        
        _delay_ms(50);
        
        AT91C_BASE_PIOA -> PIO_CODR |= (1 << 7);
        
        _delay_ms(50);
        
    }
    
}

void _delay_ms(unsigned long time) {
    
    for (volatile unsigned int i = 0; (i < (F_CPU / 10250) * (time)); i ++);
    
}