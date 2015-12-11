#define __private_include__

#include <at45/at45.h>

#include <spi/spi.h>

#include <platform/at91sam.h>

void at45_configure(void) {
	
	/* ~ Configure the CS pin. ~ */
	
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_PER);
	
    /* ~ Enable the CS pin as an input. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODR);
    
    /* ~ Enable the CS pin pull-up. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_PPUER);
	
}

void at45_enable(void) {
	
    spi_enable();
    
    /* ~ Enable the CS pin as an output. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_OER);
    
    /* Pull the CS pin low to enable the device. */
	
	set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_CODR);
	
}

void at45_disable(void) {
    
    spi_disable();
	
    /* Configure the external at45 memory chip's CS pin as an input. */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_ODR);
    
    /* ~ Enable the CS pin pull-up resistor. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, AT91C_BASE_PIOA -> PIO_PPUER);
	
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

#define FLASH_OPCODE_CHIP_ERASE_0		0xC7

#define FLASH_OPCODE_CHIP_ERASE_1		0x94

#define FLASH_OPCODE_CHIP_ERASE_2		0x80

#define FLASH_OPCODE_CHIP_ERASE_3		0x9A

extern void at45_wait(void);

void at45_format(void) {
	
	/* Disable interrupts to prevent memory corruption. */
	
	disable_interrupts();
	
	/* Wait until the flash chip is ready to recieve data. */
	
	at45_wait();
	
	/* ~ Reset the device to prepare it for the incoming opcode. ~ */
	
	at45_reset();
	
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
	
	at45_wait();
	
	/* ~ Disable the device so that no data can be recieved until the next opcode is sent. ~ */
	
	at45_disable();
	
	/* Enable interrupts again. */
	
	enable_interrupts();
	
}