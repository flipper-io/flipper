#define __private_include__

#include <platform/at91sam.h>

#include <platform/io.h>

void digital_set_direction(uint8_t pin, uint8_t direction) {
	
	/* ~ Enable the digital IO pin. ~ */
	
	set_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_PER);
	
	switch (direction) {
			
		case OUTPUT:
			
			/* ~ Disable the pin as an input. ~ */
			
			clear_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_ODR);
			
			/* ~ Configure the pin as an output. ~ */
			
			set_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_OER);
			
			break;
			
		case INPUT:
			
			/* ~ Disable the pin as an output. ~ */
			
			clear_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_OER);
			
			/* ~ Configure the pin as an input. ~ */
			
			set_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_ODR);
			
			break;
			
	}
	
}

void digital_write(uint8_t pin, uint16_t value) {
	
	switch (value) {
			
		case true:
			
			/* ~ Clear the current off state of the pin. ~ */
			
			clear_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_CODR);
			
			/* ~ Turn the digital IO pin on. ~ */
			
			set_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_SODR);
			
			break;
			
		case false:
			
			/* ~ Clear the current on state of the pin. ~ */
			
			clear_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_SODR);
			
			/* ~ Turn the digital IO pin off. ~ */
			
			set_bit_in_port(pin, AT91C_BASE_PIOA -> PIO_CODR);
			
			break;
			
	}
	
}

bool digital_read(uint8_t pin) {
	
	/* ~ Read the ON/OFF state of the digital IO pin from the PIOA. ~ */
	
	return get_bit_from_port(pin, (AT91C_BASE_PIOA -> PIO_PDSR));
	
}

void digital_pulse(uint8_t pin, uint16_t cycle) {
	
	
	
}