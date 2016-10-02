#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>
#include <platform/atsam4s16b.h>

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

/* See utils/copy.s for the source of this applet. These are the raw thumb instructions that result from the compilation of the applet. */
uint8_t applet[] = {
	0x5F, 0xEA, 0x0E, 0x04,
	0x05, 0x48, 0x06, 0x49,
	0x06, 0x4A, 0x03, 0xE0,
	0x08, 0xC9, 0x08, 0xC0,
	0xA2, 0xF1, 0x01, 0x02,
	0x00, 0x2A, 0xF9, 0xD1,
	0x20, 0x47, 0x00, 0xBF,
	0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00,
	0x80, 0x00, 0x00, 0x00,
	0xAF, 0xF3, 0x00, 0x80,
	0xAF, 0xF3, 0x00, 0x80
};

/* Place the applet in RAM somewhere far away from the region used by the SAM-BA. */
#define _APPLET IRAM_ADDR + 0x1000
#define _APPLET_DESTINATION _APPLET + 0x30
#define _APPLET_SOURCE _APPLET + 0x34
#define _PAGEBUFFER IRAM_ADDR + 0x2000

/* Instructs the SAM-BA to jump to the given address. */
void sam_ba_jump(uint32_t address) {
	char buffer[11];
	sprintf(buffer, "G%08X#", address);
	uart.push(buffer, sizeof(buffer) - 1);
}

/* Instructs the SAM-BA to write a word to the address provided. */
void sam_ba_write_word(uint32_t destination, uint32_t word) {
	char buffer[20];
	sprintf(buffer, "W%08X,%08X#", destination, word);
	uart.push(buffer, sizeof(buffer) - 1);
}

/* Instructs the SAM-BA to read a byte from the address provided. */
uint8_t sam_ba_read_byte(uint32_t source) {
	char buffer[12];
	sprintf(buffer, "o%08X,#", source);
	uart.push(buffer, sizeof(buffer) - 1);
	return uart.get();
}

/* Instructs the SAM-BA to read a word from the address provided. */
uint32_t sam_ba_read_word(uint32_t source) {
	char buffer[12];
	sprintf(buffer, "w%08X,#", source);
	uart.push(buffer, sizeof(buffer) - 1);
	uint32_t result = 0;
	uart.pull(&result, sizeof(uint32_t));
	return result;
}

/* Writes the given command and page number the EEFC -> FCR register. */
void sam_ba_write_efc_fcr(uint8_t command, uint32_t page) {
	sam_ba_write_word(0x400E0A04U, (EEFC_FCR_FKEY(0x5A) | EEFC_FCR_FARG(page) | EEFC_FCR_FCMD(command)));
}

/* Moves data from the host to the device's RAM using the SAM-BA and XMODEM protocol. */
int sam_ba_copy(uint32_t destination, void *source, uint32_t length) {
	/* Initialize the transfer. */
	char buffer[20];
	sprintf(buffer, "S%08X,%08X#", destination, length);
	uart.push(buffer, sizeof(buffer) - 1);
	/* Check for the clear to send byte. */
	if (uart.get() != 'C') {
		return lf_error;
	}
	/* Calculate the number of packets needed to perform the transfer. */
	int packets = lf_ceiling(length, XLEN);
	for (int packet = 0; packet < packets; packet ++) {
		uint32_t _len = XLEN;
		if (length < _len) {
			_len = length;
		}
		/* Construct the XMODEM packet. */
		struct _xpacket _packet = { SOH, (packet + 1), ~(packet + 1), { 0 }, 0x00 };
		/* Copy the chunk of data into the packet. */
		memcpy(_packet.data, (void *)(source + (packet * XLEN)), _len);
		/* Calculate the checksum of the data and write it to the packet in little endian format. */
		_packet.checksum = little(lf_checksum(_packet.data, sizeof(_packet.data)));
		/* Transfer the packet to the SAM-BA. */
		uart.push(&_packet, sizeof(struct _xpacket));
		/* Obtain acknowledgement. */
		if (uart.get() != ACK) {
			return lf_error;
		}
		/* Decrement the length appropriately. */
		length -= _len;
	}
	/* Send end of transmission. */
	uart.put(EOT);
	/* Obtain acknowledgement. */
	if (uart.get() != ACK) {
		return lf_error;
	}
	return lf_success;
}

void *load_page_data(FILE *firmware, size_t size) {
	size_t pages = lf_ceiling(size, IFLASH_PAGE_SIZE);
	uint8_t *raw = (uint8_t *)malloc(pages * IFLASH_PAGE_SIZE);
	for (int i = 0; i < pages; i ++) {
		for (int j = 0; j < IFLASH_PAGE_SIZE; j ++) {
			uint8_t c = fgetc(firmware);
			if (!feof(firmware)) {
				raw[((i * IFLASH_PAGE_SIZE) + j)] = c;
			} else {
				raw[((i * IFLASH_PAGE_SIZE) + j)] = 0;
			}
		}

	}
	/* Close the file. */
	fclose(firmware);
	return raw;
}

int main(int argc, char *argv[]) {

	/* Ensure the correct argument count. */
	if (argc < 2) {
		fprintf(stderr, "Please provide a path to the firmware.\n");
		return EXIT_FAILURE;
	}

	/* Open the firmware image. */
	FILE *firmware = fopen(argv[1], "rb");
	if (!firmware) {
		fprintf(stderr, "The file being opened, '%s', does not exist.\n", argv[1]);
		return EXIT_FAILURE;
	}

	/* Open the applet image. */
	FILE *applet_f = fopen(argv[2], "rb");
	if (!applet_f) {
		fprintf(stderr, "The file being opened, '%s', does not exist.\n", argv[2]);
		return EXIT_FAILURE;
	}

	/* Attach to a Flipper device. */
	flipper.attach();
	uart.enable();

	/* Determine the size of the file. */
	fseek(firmware, 0L, SEEK_END);
	size_t firmware_size = ftell(firmware);
	fseek(firmware, 0L, SEEK_SET);

	/* Determine the size of the applet. */
	fseek(applet_f, 0L, SEEK_END);
	size_t applet_s = ftell(applet_f);
	fseek(applet_f, 0L, SEEK_SET);

	/* Obtain a linear buffer of the applet precalculated by page. */
	void *applet_d = load_page_data(applet_f, applet_s);

	printf("Entering DFU mode.\n");
	/* Enter DFU mode. */
	cpu.dfu();
	for (int i = 0; i < 3; i ++) {
		/* Send the synchronization character. */
		uart.put('#');
		char ack[3];
		/* Check for acknowledgement. */
		uart.pull(ack, sizeof(ack));
		if (!memcmp(ack, (char []){ 0x0a, 0x0d, 0x3e }, sizeof(ack))) {
			fprintf(stderr, KGRN " Successfully entered update mode.\n" KNRM);
			goto connected;
		}
	}
	/* If no acknowledgement was received, throw and error. */
	fprintf(stderr, KRED "Failed to enter update mode.\n");
	return EXIT_FAILURE;

connected:

	/* Set normal mode. */
	printf("Entering normal mode.\n");
	uart.push("N#", 2);
	char ack[2];
	uart.pull(ack, sizeof(ack));
	if (memcmp(ack, (char []){ 0x0A, 0x0D }, sizeof(ack))) {
		fprintf(stderr, "Failed to enter normal mode.\n");
		goto done;
	}
	printf(KGRN " Successfully entered normal mode.\n" KNRM);


	printf("Uploading copy applet.\n");
	/* Move the copy applet into RAM. */
	int _e = sam_ba_copy(_APPLET, applet_d, sizeof(applet));
	if (_e < lf_success) {
		fprintf(stderr, KRED "Failed to upload copy applet.\n" KNRM);
		goto done;
	}
	printf(KGRN " Successfully uploaded copy applet.\n" KNRM);

	/* Write the source of the page data into the applet. */
	sam_ba_write_word(_APPLET + _APPLET_SOURCE, _PAGEBUFFER);

	/* Obtain a linear buffer of the firmware precalculated by page. */
	void *pagedata = load_page_data(firmware, firmware_size);

	/* Set flash wait states. */
	sam_ba_write_word(0x400E0A00, EEFC_FMR_FWS(5));

	/* Calculate the number of pages to send. */
	lf_size_t pages = lf_ceiling(firmware_size, IFLASH_PAGE_SIZE);
	/* Send the firmware, page by page. */
	printf("Uploading page ");
	for (lf_size_t page = 0; page < pages; page ++) {
		char buf[32];
		/* Print the page count. */
		sprintf(buf, "%i / %u.", page + 1, pages);
		printf("%s", buf);
		fflush(stdout);
		/* Copy the page. */
		int _e = sam_ba_copy(_PAGEBUFFER, (void *)(pagedata + (page * IFLASH_PAGE_SIZE)), IFLASH_PAGE_SIZE);
		if (_e < lf_success) {
			fprintf(stderr, KRED "\nFailed to upload page %i of %i.\n" KNRM, page + 1, pages);
			goto done;
		}

		//printf("Jumping to 0x%08x\n", _PAGEBUFFER + 1);
		// sam_ba_write_word(0x400E0E00U, (1 << 8)); // PER
		// sam_ba_write_word(0x400E0E10U, (1 << 8)); // OER
		// sam_ba_write_word(0x400E0E30U, (1 << 8)); // SODR
        // sam_ba_write_word(0x020010000, 0xeaea4f4f);
		// sam_ba_jump(_PAGEBUFFER + 1);
        // uint32_t sus[4];
        // sus[0] = sam_ba_read_byte(0x020010000);
        // sus[1] = sam_ba_read_byte(0x020010001);
        // sus[2] = sam_ba_read_byte(0x020010002);
        // sus[3] = sam_ba_read_byte(0x020010003);
        // printf("\nsus: %08x\n", *(uint32_t *)sus);

		/* Write the destination. */
		sam_ba_write_word(_APPLET + _APPLET_DESTINATION, (uint32_t)(IFLASH_ADDR + (page * IFLASH_PAGE_SIZE)));
		/* Jump to the copy applet to move the page into the flash write buffer. */
		sam_ba_jump(_APPLET + 1);
		/* Erase and write the page into flash. */
		sam_ba_write_word(IFLASH_ADDR, 0xdeadbeef);
		sam_ba_write_word(IFLASH_ADDR+4, 0xdeafbabe);
		sam_ba_write_efc_fcr(0x03, page);
		/* Clear the progress message. */
		if (page != pages - 1) {
			for (int j = 0; j < strlen(buf); j ++) printf("\b");
			printf("\n");
			fflush(stdout);
		}
	}

	/* Read the contents of flash. */
	printf("\n\n");
	for (int i = 0; i < 8; i ++) {
		uint32_t word = sam_ba_read_word(IFLASH_ADDR + (i * sizeof(uint32_t)));
		printf("0x%08x\n", word);
	}

	printf(KGRN "\n Successfully uploaded new firmware.\n" KNRM);

	printf("Resetting the CPU.\n");
	/* Reset the CPU. */
	cpu.reset();
	printf(KGRN " Successfully reset the CPU.\n" KNRM);

done:
	/* Free the memory allocated to hold the page data. */
	free(pagedata);

    return EXIT_SUCCESS;
}
