#define __private_include__

#include <flash/flash.h>

#include <platform/atmega.h>

#include <spi/spi.h>

#include <led/led.h>

void flash_configure(void) {
	
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

void flash_enable(void) {
	
    spi_enable();
    
    /* Configure the external flash memory chip's CS pin as an output. */
    
    set_bit_in_port(FLASH_CS_PIN, FLASH_CS_DDR);
    
	/* Pull the CS pin low to enable the device. */
	
	clear_bit_in_port(FLASH_CS_PIN, FLASH_CS_PORT);
	
}

void flash_disable(void) {
	
    spi_disable();
    
    /* Configure the external flash memory chip's CS pin as an input. */
    
    clear_bit_in_port(FLASH_CS_PIN, FLASH_CS_DDR);
	
    /* ~ Enable the CS pin pull-up resistor. ~ */
    
    set_bit_in_port(FLASH_CS_PIN, FLASH_CS_PORT);
    
}

void flash_reset(void) {
    
    spi_enable();
    
    /* Configure the external flash memory chip's CS pin as an output. */
    
    set_bit_in_port(FLASH_CS_PIN, FLASH_CS_DDR);
	
	/* Pull the CS pin high to disable the device. */
	
	set_bit_in_port(FLASH_CS_PIN, PORTB);
	
	/* Pull the CS pin low. This issues a command decoder reset and puts the device into an idle state. */
	
	clear_bit_in_port(FLASH_CS_PIN, PORTB);
	
}

/*
 
 Note that flash_alloc() and flash_free() are defined in the common 'alloc.c' for organizational purposes.
 
*/

#define FLASH_OPCODE_CHIP_ERASE_0 0xC7

#define FLASH_OPCODE_CHIP_ERASE_1 0x94

#define FLASH_OPCODE_CHIP_ERASE_2 0x80

#define FLASH_OPCODE_CHIP_ERASE_3 0x9A

extern void flash_wait(void);

void flash_format(void) {
	
#if false
    
    /* !!!!!!!! THIS CODE APPEARS TO BE EVIL AND BRICKS THE FLASH CHIP! *cries* !!!!!!!! */
    
	/* Disable interrupts to prevent memory corruption. */
	
	disable_interrupts();
	
	/* ~ Indicate that we are busy. ~ */
	
	led_set_rgb(LED_COLOR_BUSY);
	
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
	
	/* Wait until the flash chip has been erased. */
	
	flash_wait();
	
	/* ~ Disable the device so that no data can be recieved until the next opcode is sent. ~ */
	
	flash_disable();
	
	/* ~ Indicate that the operation was successful. ~ */
	
	led_set_rgb(LED_COLOR_SUCCESS);
	
	/* Enable interrupts again. */
	
	enable_interrupts();
	
#endif
    
}