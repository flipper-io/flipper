#define __private_include__

#include <at45/at45.h>

#include <platform/atmega.h>

#include <spi/spi.h>

#include <led/led.h>

void at45_configure(void) {
	
	/* Configure the external flash memory chip's CS pin as an input. */
	
	clear_bit_in_port(FLASH_CS_PIN, FLASH_CS_DDR);
    
    /* ~ Enable the CS pin pull-up resistor. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, FLASH_CS_PORT);

	/* Configure the external flash memory chip's reset pin as an output. */
	
	set_bit_in_port(FLASH_RESET_PIN, FLASH_RESET_DDR);
	
	/* Pull the reset pin high. (This is its default, inactive state.) */
	
	set_bit_in_port(FLASH_RESET_PIN, FLASH_RESET_PORT);
	
	/* Configure the external flash memory chip's WP pin as an output. */
	
	set_bit_in_port(FLASH_WP_PIN, FLASH_WP_DDR);
	
	/* Pull the WP pin high. (This is its default, inactive state.) */
	
	set_bit_in_port(FLASH_WP_PIN, FLASH_WP_PORT);
	
}

void at45_enable(void) {
	
    spi_enable();
    
    /* Configure the external flash memory chip's CS pin as an output. */
    
    set_bit_in_port(FLASH_CS_PIN, FLASH_CS_DDR);
    
	/* Pull the CS pin low to enable the device. */
	
	clear_bit_in_port(FLASH_CS_PIN, FLASH_CS_PORT);
	
}

void at45_disable(void) {
	
    spi_disable();
    
    /* Configure the external flash memory chip's CS pin as an input. */
    
    clear_bit_in_port(FLASH_CS_PIN, FLASH_CS_DDR);
	
    /* ~ Enable the CS pin pull-up resistor. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, FLASH_CS_PORT);
    
}

void at45_reset(void) {
    
    spi_enable();
    
    /* Configure the external flash memory chip's CS pin as an output. */
    
    set_bit_in_port(FLASH_CS_PIN, FLASH_CS_DDR);
	
	/* Pull the CS pin high to disable the device. */
	
	set_bit_in_port(FLASH_CS_PIN, FLASH_CS_PORT);
	
	/* Pull the CS pin low. This issues a command decoder reset and puts the device into an idle state. */
	
	clear_bit_in_port(FLASH_CS_PIN, FLASH_CS_PORT);
	
}

/*
 
 Note that at45_alloc() and at45_free() are defined in the common 'alloc.c' for organizational purposes.
 
*/

#define FLASH_OPCODE_CHIP_ERASE_0 0xC7

#define FLASH_OPCODE_CHIP_ERASE_1 0x94

#define FLASH_OPCODE_CHIP_ERASE_2 0x80

#define FLASH_OPCODE_CHIP_ERASE_3 0x9A

extern void at45_wait(void);

void at45_format(void) {
	
	
    
}