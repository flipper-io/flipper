#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>
#include <platform/atsam4s16b.h>

#define BOARD_OSCOUNT   (CKGR_MOR_MOSCXTST(0x8))
#define BOARD_PLLBR     (CKGR_PLLBR_MULB(0x7) \
                       | CKGR_PLLBR_PLLBCOUNT(0x1) \
                       | CKGR_PLLBR_DIVB(0x1))
#define BOARD_MCKR      (PMC_MCKR_PRES_CLK_2 | PMC_MCKR_CSS_PLLB_CLK)

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
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x09, 0x48, 0x0A, 0x49, 0x0A, 0x4A, 0x02, 0xE0, 0x08, 0xC9, 0x08, 0xC0, 0x01, 0x3A, 0x00, 0x2A, 0xFA, 0xD1, 0x08, 0x48, 0x09, 0x49, 0x01, 0x60, 0x07, 0x48, 0x00, 0x68, 0x01, 0x21, 0x08, 0x40, 0x88, 0x42, 0xF9, 0xD1, 0x70, 0x47, 0x00, 0xBF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x80, 0x00, 0x00, 0x00, 0x04, 0x0A, 0x0E, 0x40, 0x08, 0x0A, 0x0E, 0x40, 0x03, 0x00, 0x00, 0x5A
};

/* Place the applet in RAM somewhere far away from the region used by the SAM-BA. */
#define _APPLET IRAM_ADDR + 0x800
#define _APPLET_STACK _APPLET
#define _APPLET_ENTRY _APPLET + 0x04
#define _APPLET_DESTINATION _APPLET + 0x30
#define _APPLET_SOURCE _APPLET + 0x34
#define _APPLET_WORDS _APPLET + 0x38
#define _PAGEBUFFER _APPLET + 0x100

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

/* Instructs the SAM-BA to write a byte from the address provided. */
void sam_ba_write_byte(uint32_t destination, uint8_t byte) {
	char buffer[20];
	sprintf(buffer, "O%08X,%02X#", destination, byte);
	uart.push(buffer, sizeof(buffer) - 1);
}

/* Instructs the SAM-BA to read a word from the address provided. */
uint32_t sam_ba_read_word(uint32_t source) {
//	char buffer[12];
//	sprintf(buffer, "w%08X,#", source);
//	uart.push(buffer, sizeof(buffer) - 1);
//	uint32_t result = 0;
//	uart.pull(&result, sizeof(uint32_t));
	uint8_t ret[4];
	ret[0] = sam_ba_read_byte(source);
	ret[1] = sam_ba_read_byte(source + 1);
	ret[2] = sam_ba_read_byte(source + 2);
	ret[3] = sam_ba_read_byte(source + 3);
	return *(uint32_t *)ret;
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

	/* Attach to a Flipper device. */
	flipper.attach();

    /* Open the firmware image. */
    FILE *firmware = fopen(argv[1], "rb");
    if (!firmware) {
        fprintf(stderr, "The file being opened, '%s', does not exist.\n", argv[1]);
        return EXIT_FAILURE;
    }

	/* Determine the size of the file. */
	fseek(firmware, 0L, SEEK_END);
	size_t firmware_size = ftell(firmware);
	fseek(firmware, 0L, SEEK_SET);

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
        return EXIT_FAILURE;
    }
    printf(KGRN " Successfully entered normal mode.\n" KNRM);

    printf("Checking security bit.\n");
	sam_ba_write_efc_fcr(0x0D, 0);
	if (sam_ba_read_word(0x400E0A0C) & 0x01) {
		fprintf(stderr, KRED "The device's security bit is set. Please erase again.\n");
		return EXIT_FAILURE;
	}
    printf(KGRN " Security bit is clear.\n" KNRM);

	printf("Uploading copy applet.\n");
	/* Move the copy applet into RAM. */
	int _e = sam_ba_copy(_APPLET, applet, sizeof(applet));
	if (_e < lf_success) {
		fprintf(stderr, KRED "Failed to upload copy applet.\n" KNRM);
		return EXIT_FAILURE;
	}
	printf(KGRN " Successfully uploaded copy applet.\n\n" KNRM);

    /* Write the stack address into the applet. */
    sam_ba_write_word(_APPLET_STACK, IRAM_ADDR + IRAM_SIZE);
    /* Write the entry address into the applet. */
    sam_ba_write_word(_APPLET_ENTRY, _APPLET + 0x09);
	/* Write the source of the page data into the applet. */
	sam_ba_write_word(_APPLET_SOURCE, _PAGEBUFFER);

	/* Obtain a linear buffer of the firmware precalculated by page. */
	void *pagedata = load_page_data(firmware, firmware_size);

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
        /* Write the destination of the page data into the applet. */
		sam_ba_write_word(_APPLET_DESTINATION, IFLASH_ADDR + (page * IFLASH_PAGE_SIZE));
        /* Execute the applet to load the page into flash. */
        sam_ba_jump(_APPLET);
		/* Clear the progress message. */
		if (page != pages - 1) {
			for (int j = 0; j < strlen(buf); j ++) printf("\b");
			printf("\n");
			fflush(stdout);
		}
	}

    // printf("\n\n");
	// for (int i = 0; i < lf_ceiling(firmware_size, sizeof(uint32_t)); i ++) {
	// 	uint32_t word = sam_ba_read_word(IFLASH_ADDR + (i * sizeof(uint32_t)));
	// 	uint32_t _word = *(uint32_t *)(pagedata + (i * sizeof(uint32_t)));
	// 	printf("0x%08x : 0x%08x -> %s\n", word, _word, (word == _word) ? "GOOD" : "BAD");
	// }

	printf("\n\nResetting the CPU.\n");
	/* Reset the CPU. */
	cpu.reset();
	printf(KGRN " Successfully reset the CPU.\n" KNRM);

    printf(KGRN "\nSuccessfully uploaded new firmware.\n" KNRM);

done:
	/* Free the memory allocated to hold the page data. */
	free(pagedata);

    return EXIT_SUCCESS;
}
