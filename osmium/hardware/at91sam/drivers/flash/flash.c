#define __private_include__

#include <flash/flash.h>

#include <spi/spi.h>

#include <platform/at91sam.h>

void flash_configure(void) {
	
	/* ~ Configure the CS pin. ~ */
	
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_PER);
	
    /* ~ Enable the CS pin as an input. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODR);
    
    /* ~ Enable the CS pin pull-up. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_PPUER);
	
}

void flash_enable(void) {
	
    spi_enable();
    
    /* ~ Enable the CS pin as an output. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);
    
    /* Pull the CS pin low to enable the device. */
	
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_CODR);
	
}

void flash_disable(void) {
    
    spi_disable();
	
    /* Configure the external flash memory chip's CS pin as an input. */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODR);
    
    /* ~ Enable the CS pin pull-up resistor. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_PPUER);
	
}

void flash_reset(void) {
	
    spi_enable();
    
    /* ~ Enable the CS pin as an output. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);
    
    /* ~ Turn the CS pin on. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_SODR);
    
    /* ~ Turn the CS pin off. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_CODR);
	
}

/*
 
 Note that flash_alloc() and flash_free() are defined in the common 'alloc.c' for organizational purposes.
 
*/

#define FLASH_OPCODE_CHIP_ERASE_0		0xC7

#define FLASH_OPCODE_CHIP_ERASE_1		0x94

#define FLASH_OPCODE_CHIP_ERASE_2		0x80

#define FLASH_OPCODE_CHIP_ERASE_3		0x9A

extern void flash_wait(void);

void flash_format(void) {
	
	/* Disable interrupts to prevent memory corruption. */
	
	disable_interrupts();
	
	/* Wait until the flash chip is ready to recieve data. */
	
	flash_wait();
	
	/* ~ Reset the device to prepare it for the incoming opcode. ~ */
	
	flash_reset();
	
	/* Send the appropriate opcodes to initialize a chip erase. */
	
	spi_put(FLASH_OPCODE_CHIP_ERASE_0);
	
	spi_put(FLASH_OPCODE_CHIP_ERASE_1);
	
	spi_put(FLASH_OPCODE_CHIP_ERASE_2);
	
	spi_put(FLASH_OPCODE_CHIP_ERASE_3);
	
	/* Wait the bulk of the erase cycle period so as to not overload the SPI. */
	
	delay_seconds(5);
	
	delay_seconds(5);
	
	delay_seconds(5);
	
	delay_seconds(5);
	
	delay_seconds(5);
	
	delay_seconds(5);
	
	delay_seconds(5);
	
	/* Wait until the flash chip has been erased. */
	
	flash_wait();
	
	/* ~ Disable the device so that no data can be recieved until the next opcode is sent. ~ */
	
	flash_disable();
	
	/* Enable interrupts again. */
	
	enable_interrupts();
	
}