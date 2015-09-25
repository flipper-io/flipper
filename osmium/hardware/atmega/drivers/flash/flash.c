#define __private_include__

#include <flash/flash.h>

#include <platform/atmega.h>

#include <spi/spi.h>

void flash_configure(void) {

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
	
	/* Pull the CS pin low to enable the device. */
	
	clear_bit_in_port(FLASH_CS_PIN, PORTB);
	
}

void flash_disable(void) {
	
	/* Pull the CS pin high to disable the device. */
	
	set_bit_in_port(FLASH_CS_PIN, PORTB);
	
}

void flash_reset(void) {
	
	/* Pull the CS pin high to disable the device. */
	
	set_bit_in_port(FLASH_CS_PIN, PORTB);
	
	/* Pull the CS pin low. This issues a command decoder reset and puts the device into an idle state. */
	
	clear_bit_in_port(FLASH_CS_PIN, PORTB);
	
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

void flash_push(void *source, uint32_t length, fsp destination) {
	
	/* ~ Calculate the page of the address. ~ */
	
	uint16_t page = destination / 528;
	
	/* ~ Calculate the destination of the address. ~ */
	
	uint16_t offset = destination % 528;
	
	/* ~ Move the data from the page into the buffer. ~ */
	
	flash_transfer_page_to_buffer_with_erase(page, 0);
	
	/* ~ Select the buffer to begin the write operation. ~ */
	
	flash_begin_writing_to_buffer_with_offset(0, offset);
	
	/* ~ Transfer the data into flash memory. ~ */
	
	while (length --) {
		
		/* ~ Move the data into the buffer. ~ */
		
		spi_put(*(uint8_t *)(source ++));
		
		/* Increment the offset. */
		
		offset ++;
		
		/* ~ If we've reached the end of the page, move to the next one. ~ */
		
		if (offset % 528 == 0) {
			
			/* ~ Reset the offset. ~ */
			
			offset = 0;
			
			/* ~ Write the data into the completed page. ~ */
			
			flash_transfer_buffer_to_page_with_erase(0, page, true);
			
			/* ~ Increment the page. ~ */
			
			page ++;
			
			/* ~ Move the data from the next page into the buffer. ~ */
			
			flash_transfer_page_to_buffer_with_erase(page, 0);
			
			/* ~ Select the new buffer. Toggling them like this makes the write process go faster? ~ */
			
			flash_begin_writing_to_buffer_with_offset(0, offset);
			
		}
		
	}
	
	/* ~ Transfer the buffer into the final page. ~ */
	
	flash_transfer_buffer_to_page_with_erase(0, page, true);
	
	/* ~ Close the connection. ~ */
	
	flash_disable();
	
}

void flash_pull(void *destination, uint32_t length, fsp source) {
	
	/* ~ Calculate the page of the address. ~ */
	
	uint16_t page = source / 528;
	
	/* ~ Calculate the offset of the address. ~ */
	
	uint16_t offset = source % 528;
	
	/* ~ Begin a continuous memory array read. ~ */
	
	flash_begin_continuous_read(page, offset);
	
	/* ~ Read the data in from memory. ~ */
	
	while (length --) *(uint8_t *)(destination ++) = spi_get();
	
	/* ~ Close the connection. ~ */
	
	flash_disable();
	
}

void *flash_dereference(fsp source, uint32_t length) {
	
	/* ~ Allocate local memory to service the request. ~ */
	
	void *destination = malloc(length);
	
	/* ~ Move the data from external memory to internal memory. ~ */
	
	flash_pull(destination, length, source);
	
	/* ~ Return a pointer to the local memory region. ~ */
	
	return destination;
	
}