#include <unistd.h>
#include <flipper.h>

#define __SAM4S16B__
#include <flipper/atsam4s/atsam4s.h>

/* Defines the XMODEM flow control bytes. */
#define SOH 0x01
#define EOT 0x04
#define ACK 0x06
#define NAK 0x15
#define ETB 0x17
#define CAN 0x18
#define XLEN 128

/* Defines the layout of an XMODEM packet. */
struct __attribute__((__packed__)) _xpacket {
	uint8_t header;
	uint8_t number;
	uint8_t _number;
	uint8_t data[XLEN];
	uint16_t checksum;
};

/* See utils/copy_x.s for the source of this applet. These are the raw thumb instructions that result from the compilation of the applet. */
uint8_t applet[] = {
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x09, 0x48, 0x0A, 0x49,
	0x0B, 0x4A, 0x02, 0xE0,
	0x08, 0xC9, 0x08, 0xC0,
	0x01, 0x3A, 0x00, 0x2A,
	0xFA, 0xD1, 0x09, 0x48,
	0x0A, 0x49, 0x06, 0x4A,
	0x11, 0x43, 0x01, 0x60,
	0x70, 0x47, 0x00, 0xBF,
	0xAF, 0xF3, 0x00, 0x80,
	0xAF, 0xF3, 0x00, 0x80,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x80, 0x00, 0x00, 0x00,
	0x04, 0x0A, 0x0E, 0x40,
	0x08, 0x0A, 0x0E, 0x40,
	0x01, 0x00, 0x00, 0x5A
};

/* Place the applet in RAM somewhere far away from the region used by the SAM-BA. */
#define _APPLET IRAM_ADDR + 0x800
#define _APPLET_STACK _APPLET
#define _APPLET_ENTRY _APPLET + 0x04
#define _APPLET_DESTINATION _APPLET + 0x30
#define _APPLET_SOURCE _APPLET_DESTINATION + 0x04
#define _APPLET_PAGE _APPLET_SOURCE + 0x04
#define _APPLET_WORDS _APPLET_PAGE + 0x04
#define _PAGEBUFFER _APPLET + sizeof(applet)

#define EFC_CLB 0x09
#define EFC_SGPB 0x0B
#define EFC_GGPB 0x0D

#define REGADDR(reg) ((uint32_t)(uintptr_t)&reg)

/* Defines the number of times communication will be retried. */
#define RETRIES 4

/* Instructs the SAM-BA to jump to the given address. */
void sam_ba_jump(uint32_t address) {
	char buffer[11];
	sprintf(buffer, "G%08X#", address);
	uart0_push(buffer, sizeof(buffer) - 1);
}

/* Instructs the SAM-BA to write a word to the address provided. */
void sam_ba_write_word(uint32_t destination, uint32_t word) {
	char buffer[20];
	sprintf(buffer, "W%08X,%08X#", destination, word);
	uart0_push(buffer, sizeof(buffer) - 1);
}

/* Instructs the SAM-BA to read a byte from the address provided. */
uint8_t sam_ba_read_byte(uint32_t source) {
	char buffer[12];
	sprintf(buffer, "o%08X,#", source);
	uart0_push(buffer, sizeof(buffer) - 1);
	uint32_t retries = 0;
	while(!uart0_ready() && retries ++ < 8);
	return uart0_get();
}

/* Instructs the SAM-BA to write a byte from the address provided. */
void sam_ba_write_byte(uint32_t destination, uint8_t byte) {
	char buffer[20];
	sprintf(buffer, "O%08X,%02X#", destination, byte);
	uart0_push(buffer, sizeof(buffer) - 1);
}

/* Instructs the SAM-BA to read a word from the address provided. */
uint32_t sam_ba_read_word(uint32_t source) {
	char buffer[12];
	sprintf(buffer, "w%08X,#", source);
	uart0_push(buffer, sizeof(buffer) - 1);
	uint8_t retries = 0;
	while(!uart0_ready() && retries ++ < 8);
	uint32_t result = 0;
	uart0_pull(&result, sizeof(uint32_t));
	return result;
}

/* Drains the data in the uart0 buffer. */
void uart0_drain(void) {
	while (!uart0_ready()) (void)uart0_get();
}

/* Writes the given command and argument into the EFC0->EEFC_FCR register. */
void sam_ba_write_efc_fcr(uint8_t command, uint32_t arg) {
	sam_ba_write_word(REGADDR(EFC0->EEFC_FCR), (EEFC_FCR_FKEY_PASSWD | EEFC_FCR_FARG(arg) | command));
}

/* Moves data from the host to the device's RAM using the SAM-BA and XMODEM protocol. */
int sam_ba_copy(uint32_t destination, void *source, uint32_t length) {

	char buffer[20];
	uart0_reset();
	sprintf(buffer, "S%08X,%08X#", destination, length);
	uart0_push(buffer, sizeof(buffer) - 1);

	/* Calculate the number of packets needed to perform the transfer. */
	int packets = lf_ceiling(length, XLEN);
	for (int packet = 0; packet < packets; packet ++) {


		uart0_reset();

		uint32_t _len = XLEN;
		if (length < _len) {
			_len = length;
		}

		/* Construct the XMODEM packet. */
		struct _xpacket _packet = { SOH, (packet + 1), ~(packet + 1), { 0 }, 0x00 };
		/* Copy the chunk of data into the packet. */
		memcpy(_packet.data, (void *)(source + (packet * XLEN)), _len);
		/* Calculate the checksum of the data and write it to the packet in little endian format. */
		_packet.checksum = little(lf_crc(_packet.data, sizeof(_packet.data)));
		/* Transfer the packet to the SAM-BA. */
		uart0_push(&_packet, sizeof(struct _xpacket));

		lf_assert(uart0_get() == ACK, failure, E_UNIMPLEMENTED, "Failed to get ACK.");

		/* Decrement the length appropriately. */
		length -= _len;
	}

	/* Send end of transmission. */
	uart0_put(EOT);

	lf_assert(uart0_get() == ACK, failure, E_UNIMPLEMENTED, "Failed to get EOT ACK.");

	return lf_success;

failure:
	return lf_error;
}

void *load_page_data(FILE *firmware, size_t size) {
	size_t pages = lf_ceiling(size, IFLASH0_PAGE_SIZE);
	uint8_t *raw = (uint8_t *)malloc(pages * IFLASH0_PAGE_SIZE);
	for (size_t i = 0; i < pages; i ++) {
		for (size_t j = 0; j < IFLASH0_PAGE_SIZE; j ++) {
			uint8_t c = fgetc(firmware);
			if (!feof(firmware)) {
				raw[((i * IFLASH0_PAGE_SIZE) + j)] = c;
			} else {
				raw[((i * IFLASH0_PAGE_SIZE) + j)] = 0;
			}
		}

	}
	/* Close the file. */
	fclose(firmware);
	return raw;
}

int enter_normal_mode(void) {
	int tries = 0;
	char ack[2];

	do {
		uart0_reset();
		uart0_push("N#", 2);
		usleep(1000);
		lf_try(uart0_pull(ack, sizeof(ack)));
		if (!memcmp(ack, (const uint8_t []){ '\n', '\r' }, 2)) return lf_success;

		/* If we failed the first time around, enter DFU. */
		if (!tries) {
			sam_enter_dfu();
		}

	} while (tries++ < 4);

	return lf_error;
}


int main(int argc, char *argv[]) {

	void *pagedata = NULL;
	FILE *firmware = NULL;

	/* Ensure the correct argument count. */
	if (argc < 2) {
		fprintf(stderr, "Please provide a path to the firmware.\n");
		return EXIT_FAILURE;
	}

	//lf_set_debug_level(LF_DEBUG_LEVEL_ALL);

	/* Attach to a Flipper device. */
	struct _lf_device *device = flipper.attach();
	carbon_select_u2(device);

	/* Open the firmware image. */
	firmware = fopen(argv[1], "rb");
	lf_assert(firmware, failure, E_UNIMPLEMENTED, "The file being opened, '%s', does not exist.\n", argv[1]);

	/* Determine the size of the file. */
	fseek(firmware, 0L, SEEK_END);
	size_t firmware_size = ftell(firmware);
	fseek(firmware, 0L, SEEK_SET);

	/* DFU baud. */
	uart0_setbaud(DFU_BAUD);

	/* Reset the 4S. */
	sam_reset();

	/* Enter normal mode. (Values are sent as binary.)*/
	lf_debug("Entering device firmware update mode.");
	lf_assert(enter_normal_mode() == lf_success, failure, E_UNIMPLEMENTED, "Failed to enter normal mode.");
	lf_debug(KGRN " Successfully entered DFU mode." KNRM);

	/* Ensure the security bit is clear. */
	lf_debug("Checking security bit.");
	sam_ba_write_efc_fcr(EEFC_FCR_FCMD_GGPB, 0);
	lf_assert((sam_ba_read_word(REGADDR(EFC0->EEFC_FRR)) & 0x01) == 0, failure, E_UNIMPLEMENTED, KRED "The device's security bit is set." KNRM);
	lf_debug(KGRN " Security bit is clear." KNRM);

	/* Move the copy applet into RAM. */
	lf_debug("Uploading copy applet.");
	lf_assert(sam_ba_copy(_APPLET, applet, sizeof(applet)) == lf_success, failure, E_UNIMPLEMENTED, KRED "Failed to upload copy applet." KNRM);
	lf_debug(KGRN " Successfully uploaded copy applet." KNRM);

	/* Write the stack address into the applet. */
	sam_ba_write_word(_APPLET_STACK, IRAM_ADDR + IRAM_SIZE);
	/* Write the entry address into the applet. */
	sam_ba_write_word(_APPLET_ENTRY, _APPLET + 0x09);
	/* Write the destination of the page data into the applet. */
	sam_ba_write_word(_APPLET_DESTINATION, IFLASH0_ADDR);
	/* Write the source of the page data into the applet. */
	sam_ba_write_word(_APPLET_SOURCE, _PAGEBUFFER);

	/* Obtain a linear buffer of the firmware precalculated by page. */
	pagedata = load_page_data(firmware, firmware_size);

	/* Calculate the number of pages to send. */
	lf_size_t pages = lf_ceiling(firmware_size, IFLASH0_PAGE_SIZE);
	/* Send the firmware, page by page. */
	for (lf_size_t page = 0; page < pages; page ++) {
		/* Print the page count. */
		printf("Uploading page %i / %u. (%.2f%%)", page + 1, pages, ((float)(page + 1))/pages*100);
		fflush(stdout);
		/* Copy the page. */
		int _e = sam_ba_copy(_PAGEBUFFER, (void *)(pagedata + (page * IFLASH0_PAGE_SIZE)), IFLASH0_PAGE_SIZE);
		lf_assert(_e == lf_success, failure, E_UNIMPLEMENTED, KRED "Failed to upload page %i of %i." KNRM, page + 1, pages);

		/* Write the page number into the applet. */
		sam_ba_write_word(_APPLET_PAGE, EEFC_FCR_FARG(page));
		/* Execute the applet to load the page into flash. */
		sam_ba_jump(_APPLET);

		/* Wait until the EFC has finished writing the page. */
		uint8_t retries = 0, fsr = 0;
		while(!((fsr = sam_ba_read_byte(REGADDR(EFC0->EEFC_FSR))) & 1) && retries ++ < 4) {
			if (fsr & 0xE) {
				fprintf(stderr, KRED "Flash write error on page %u.\n" KNRM, page);
			}
		}

		retries = 0;
		/* Clear the progress message. */
		if (page < pages - 1) printf("\33[2K\r");
	}

	/* Print statistics about the memory usage. */
	printf(KGRN "\n Successfully uploaded all pages. %zu bytes used. (%.2f%% of flash)\n" KNRM, firmware_size, (float)firmware_size/IFLASH0_SIZE*100);

	/* Set GPNVM1 to boot from flash memory. */
	sam_ba_write_efc_fcr(EEFC_FCR_FCMD_SGPB, 0x01);

	printf("Checking GPNVM1 bit.\n");
	sam_ba_write_efc_fcr(EEFC_FCR_FCMD_GGPB, 0);
	uint8_t retries = 0;
	if (!(sam_ba_read_byte(REGADDR(EFC0->EEFC_FRR)) & (1 << 1)) && retries ++ < RETRIES) {
		if (retries > RETRIES) {
			printf(KRED " GPNVM1 bit is not set.\n" KNRM);
			return EXIT_FAILURE;
		}
		/* Set GPNVM1 to boot from flash memory. */
		sam_ba_write_efc_fcr(EEFC_FCR_FCMD_SGPB, 0x01);
		/* Read the state of the GPNVM bits. */
		sam_ba_write_efc_fcr(EEFC_FCR_FCMD_GGPB, 0);
	}
	printf(KGRN "The device's GPNVM1 bit is set.\n" KNRM);

	if (argc > 2) {
		if (!strcmp(argv[2], "verify")) {
			printf("\nVerifying flash contents.\n");
			uint32_t errors = 0, perrors = 0, total = lf_ceiling(firmware_size, sizeof(uint32_t));
			uint8_t retries = 0;
			for (uint32_t i = 0; i < total; i ++) {
				uint32_t addr = IFLASH0_ADDR + (i * sizeof(uint32_t));
				if ((i % (IFLASH0_PAGE_SIZE/sizeof(uint32_t)) == 0)) {
					printf(" Checking address 0x%08x (page %lu)->%s\n", addr, i / (IFLASH0_PAGE_SIZE/sizeof(uint32_t)), (!perrors) ? KGRN "GOOD" KNRM : KRED "BAD" KNRM);
				}
				uint32_t word = sam_ba_read_word(addr);
				uint32_t _word = *(uint32_t *)(pagedata + (i * sizeof(uint32_t)));
				uint8_t match = ((uint16_t)word == (uint16_t)_word);
				//printf("0x%08x: 0x%08x (0x%08x)->%s\n", addr, word, _word, (match) ? KGRN "GOOD" KNRM : KRED "BAD" KNRM);
				if (!match && retries < RETRIES) {
					if (retries == 0) {
						perrors ++;
						errors ++;
					}
					retries ++;
					continue;
				}
				retries = 0;
				perrors = 0;
			}
			printf("Verification complete. %s%i word errors detected.\n\n" KNRM, (errors) ? KRED : KGRN, errors);
		}
	}

	free(pagedata);

	/* Go back to FMR mode. */
	uart0_setbaud(FMR_BAUD);

	lf_debug("Resetting the CPU.");
	sam_reset();
	lf_debug(KGRN " Successfully reset the CPU.\n" KNRM "----------------------");

#warning Go back to FMR baud here.

	lf_debug(KGRN "\nSuccessfully uploaded new firmware.\n" KNRM);

	return EXIT_SUCCESS;

failure:
	free(pagedata);
	return EXIT_FAILURE;
}
