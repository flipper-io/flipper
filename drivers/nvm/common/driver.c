#define __private_include__
#include <flipper/nvm.h>
#include <flipper/spi.h>

/* Define the opcodes for the flash device. */
#define FLASH_OPCODE_GET_STATUS_REGISTER                                0xD7
#define FLASH_READY_BUSY_BIT                                            0x07
#define FLASH_OPCODE_CONFIGURE_512_BYTE_PAGE_0                          0x3D
#define FLASH_OPCODE_CONFIGURE_512_BYTE_PAGE_1                          0x2A
#define FLASH_OPCODE_CONFIGURE_512_BYTE_PAGE_2                          0x80
#define FLASH_OPCODE_CONFIGURE_512_BYTE_PAGE_3                          0xA6
#define FLASH_OPCODE_BUFFER_1_WRITE                                     0x84
#define FLASH_DATA_DIRECTON_OPCODE_BUFFER_1_TO_PAGE                     0x88
#define FLASH_DATA_DIRECTON_OPCODE_BUFFER_1_TO_PAGE_WITH_ERASE          0x83
#define FLASH_OPCODE_BUFFER_2_WRITE                                     0x87
#define FLASH_DATA_DIRECTON_OPCODE_BUFFER_2_TO_PAGE                     0x89
#define FLASH_DATA_DIRECTON_OPCODE_BUFFER_2_TO_PAGE_WITH_ERASE          0x86
#define FLASH_OPCODE_TRANSFER_PAGE_TO_BUFFER_1                          0x53
#define FLASH_OPCODE_TRANSFER_PAGE_TO_BUFFER_2                          0x55
#define FLASH_OPCODE_CONTINUOUS_READ                                    0x0B
#define FLASH_OPCODE_PAGE_READ                                          0xD2

#define nvm_payload_hi(page) (page >> 6)
#define nvm_payload_lo(page) (page << 2)

#ifdef __osmium__

uint8_t nvm_get_status(void) {

	/* Reset the device to prepare it for the incoming opcode. */
	nvm_reset();

	/* Send the read status register opcode. */
	spi_put(FLASH_OPCODE_GET_STATUS_REGISTER);

	/* Get the contents of the status register. */
	uint8_t status = spi_get();

	/* Disable the device so that no data can be recieved until the next opcode is sent. */
	nvm_disable();

	return status;

}

void nvm_wait(void) {

	/* Wait until the READY/BUSY bit of the status register is set. */
	while (!(get_bit_from_port(FLASH_READY_BUSY_BIT, nvm_get_status())));

}

void nvm_begin_continuous_read(uint16_t page, uint16_t offset) {

	/* Wait until the flash chip is ready to recieve data. */
	nvm_wait();

	/* Reset the device to prepare it for the incoming opcode. */
	nvm_reset();

	/* Continuous memory read. */
	spi_put(FLASH_OPCODE_CONTINUOUS_READ);
	spi_put(nvm_payload_hi(page));
	spi_put(nvm_payload_lo(page) | hi(offset));
	spi_put(lo(offset));

	/* Complete the opcode by sending 8 don't care bits. The device will remain busy until this operation has completed. */
	spi_put(0x00);

}

void nvm_begin_continuous_buffer_read(uint8_t buffer, uint16_t offset) {

	/* Wait until the flash chip is ready to recieve data. */
	nvm_wait();

	/* Reset the device to prepare it for the incoming opcode. */
	nvm_reset();

	/* Select the buffer. */
	switch (buffer) {

		case 1:

			spi_put(0xD4);

			break;

		default:

			spi_put(0xD6);

			break;

	}

	/* Send 8 don't care bits. */
	spi_put(0x00);

	/* Send the rest of the don't care bits, plus the high end of the offset. */
	spi_put(hi(offset));

	/* Send the low end of the offset. */
	spi_put(lo(offset));

}

void nvm_transfer_buffer_to_page_with_erase(uint8_t buffer, uint16_t page, uint8_t erase) {

	/* Wait until the flash chip is ready to recieve data. */
	nvm_wait();

	/* Reset the device to prepare it for the incoming opcode. */
	nvm_reset();

	/* Send the command. */
	switch (buffer) {

		case 1:

			if (erase)

				spi_put(FLASH_DATA_DIRECTON_OPCODE_BUFFER_1_TO_PAGE_WITH_ERASE);

			else

				spi_put(FLASH_DATA_DIRECTON_OPCODE_BUFFER_1_TO_PAGE);

			break;

		default:

			if (erase)

				spi_put(FLASH_DATA_DIRECTON_OPCODE_BUFFER_2_TO_PAGE_WITH_ERASE);

			else

				spi_put(FLASH_DATA_DIRECTON_OPCODE_BUFFER_2_TO_PAGE);

			break;

	}

	/* Follow the command with the payload. */
	spi_put(nvm_payload_hi(page));
	spi_put(nvm_payload_lo(page));

	/* Complete the transfer by sending 8 don't care bits. The device will remain busy until this operation has completed. */
	spi_put(0x00);

	/* Disable the device so that no data can be recieved until the next opcode is sent. */
	nvm_disable();

}

void nvm_transfer_page_to_buffer_with_erase(uint16_t page, uint8_t buffer) {

	/* Wait until the flash chip is ready to recieve data. */
	nvm_wait();

	/* Reset the device to prepare it for the incoming opcode. */
	nvm_reset();

	/* Send the appropriate opcode. */
	switch (buffer) {

		case 1:

			spi_put(FLASH_OPCODE_TRANSFER_PAGE_TO_BUFFER_1);

			break;

		default:

			spi_put(FLASH_OPCODE_TRANSFER_PAGE_TO_BUFFER_2);

			break;
	}

	/* Follow the command with the payload. */
	spi_put(nvm_payload_hi(page));
	spi_put(nvm_payload_lo(page));

	/* Complete the opcode by sending 8 don't care bits. The device will remain busy until this operation has completed. */
	spi_put(0x00);

	/* Disable the device so that no data can be recieved until the next opcode is sent. */
	nvm_disable();

}

void nvm_begin_writing_to_buffer_with_offset(uint8_t buffer, uint16_t offset) {

	/* Wait until the flash chip is ready to recieve data. */
	nvm_wait();

	/* Reset the device to prepare it for the incoming opcode. */

	nvm_reset();

	/* Send the appropriate opcode. */
	switch (buffer) {

		case 1:

			spi_put(FLASH_OPCODE_BUFFER_1_WRITE);

			break;

		default:

			spi_put(FLASH_OPCODE_BUFFER_2_WRITE);

			break;

	}

	/* Send 8 don't care bits. */
	spi_put(0x00);

	/* Send the rest of the don't care bits, plus the high end of the offset. */
	spi_put(hi(offset));

	/* Send the low end of the offset. */
	spi_put(lo(offset));

}

void nvm_begin_reading_from_page_with_offset(uint16_t page, uint16_t offset) {

	/* Wait until the flash chip is ready to transmit data. */
	nvm_wait();

	/* Reset the device to prepare it for the incoming opcode. */
	nvm_reset();

	/* Send the page read opcode to the device. */
	spi_put(FLASH_OPCODE_PAGE_READ);

	/* Send the page and offset. */
	spi_put(nvm_payload_hi(page));
	spi_put(nvm_payload_lo(page) | hi(offset));
	spi_put(lo(offset));

	/* Send 4 don't care bytes. */
	spi_put(0x00);
	spi_put(0x00);
	spi_put(0x00);
	spi_put(0x00);

}

void nvm_reset_settings(void) {
    
    disable_interrupts();

	/* Wait until the flash chip is ready to recieve data. */
	nvm_wait();

	/* Reset the device to prepare it for the incoming opcode. */
	nvm_reset();

	/* Send the appropriate opcodes to configure the page size. */
	spi_put(FLASH_OPCODE_CONFIGURE_512_BYTE_PAGE_0);
	spi_put(FLASH_OPCODE_CONFIGURE_512_BYTE_PAGE_1);
	spi_put(FLASH_OPCODE_CONFIGURE_512_BYTE_PAGE_2);
	spi_put(FLASH_OPCODE_CONFIGURE_512_BYTE_PAGE_3);

	/* Disable the device so that no data can be recieved until the next opcode is sent. */
	nvm_disable();
    
    enable_interrupts();

}

void nvm_push(void *source, size_t length, fsp destination) {
    
    disable_interrupts();
    
	/* Calculate the page of the address. */
	uint16_t page = destination / NVM_PAGE_SIZE;

	/* Calculate the destination of the address. */
	uint16_t offset = destination % NVM_PAGE_SIZE;

	/* Move the data from the page into the buffer. */
	nvm_transfer_page_to_buffer_with_erase(page, 0);

	/* Select the buffer to begin the write operation. */
	nvm_begin_writing_to_buffer_with_offset(0, offset);

	/* Transfer the data into flash memory. */
	while (length --) {

		/* Move the data into the buffer. */
		spi_put(*(uint8_t *)(source ++));

		/* Increment the offset. */
		offset ++;

		/* If we've reached the end of the page, move to the next one. */
		if (offset % NVM_PAGE_SIZE == 0) {

			/* Reset the offset. */
			offset = 0;

			/* Write the data into the completed page. */
			nvm_transfer_buffer_to_page_with_erase(0, page, true);

			/* Increment the page. */
			page ++;

			/* Move the data from the next page into the buffer. */
			nvm_transfer_page_to_buffer_with_erase(page, 0);

			/* Select the new buffer. Toggling them like this makes the write process go faster? */
			nvm_begin_writing_to_buffer_with_offset(0, offset);

		}

	}

	/* Transfer the buffer into the final page. */
	nvm_transfer_buffer_to_page_with_erase(0, page, true);

	/* Close the connection. */
	nvm_disable();
    
    enable_interrupts();

}

void nvm_read(fsp address) {

	nvm_begin_continuous_read(address / NVM_PAGE_SIZE, address % NVM_PAGE_SIZE);

}

uint8_t nvm_get(void) {

	return spi_get();

}

void nvm_pull(void *destination, size_t length, fsp source) {
    
    disable_interrupts();
    
	/* Calculate the page of the address. */
	uint16_t page = source / NVM_PAGE_SIZE;

	/* Calculate the offset of the address. */
	uint16_t offset = source % NVM_PAGE_SIZE;

	/* Begin a continuous memory array read. */
	nvm_begin_continuous_read(page, offset);

	/* Read the data in from memory. */
	while (length --) *(uint8_t *)(destination ++) = spi_get();

	/* Close the connection. */
	nvm_disable();
    
    enable_interrupts();

}

void *nvm_dereference(fsp source, size_t length) {

	/* Allocate local memory to service the request. */
	void *destination = malloc(length);

	/* Move the data from external memory to internal memory. */
	nvm_pull(destination, length, source);

	return destination;

}

#endif