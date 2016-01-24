#define __private_include__

#include <at45/at45.h>

#include <spi/spi.h>

#include <platform/at91sam.h>

void at45_configure(void) {
	
	/* ~ Configure the CS pin. ~ */
	
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_PER);

	/* ~ Configure the CS pin as an output. ~ */
	
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);
	
	/* ~ Configure the CS pin for single write access. ~ */
	
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OWER);
	
}

void at45_enable(void) {
	
	/* ~ Enable the SPI. ~ */
	
    spi_enable();
    
	/* ~ Pull the CS pin low to enable the device. ~ */
	
	clear_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODSR);
	
}

void at45_disable(void) {
	
	/* ~ Pull the CS pin high to disable the device. ~ */
	
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODSR);
	
	/* ~ Disable the SPI. ~ */
	
	spi_disable();
	
}

void at45_reset(void) {
	
    spi_enable();
    
    /* ~ Enable the CS pin as an output. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);
    
    /* ~ Turn the CS pin on. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_SODR);
    
    /* ~ Turn the CS pin off. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_CODR);
	
}

/*
 
 Note that at45_alloc() and at45_free() are defined in the common 'alloc.c' for organizational purposes.
 
*/

void at45_format(void) {
	
	
	
}