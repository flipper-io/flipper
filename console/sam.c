#define __private_include__
#include <flipper/flipper.h>
#include <fs/crc.h>
#include <unistd.h>
#include "console.h"

#define address(x)				((void *)(x))
#define USERSPACE				address(0x00202000)
#define SAM_STACK				address(0x0204000)
#define APPLET_OFFSET			0x01
#define _APPLET_STACK			address(USERSPACE + 0x20)
#define _APPLET_DESTINATION		address(USERSPACE + 0x28)
#define _APPLET_SOURCE			address(USERSPACE + 0x2C)
#define _APPLET_WORDS			address(USERSPACE + 0x30)
#define APPLET_SIZE				0x34
#define FLASHSPACE				address(0x00100000)
#define EFC0_FMR				address(0xFFFFFF60)
#define EFC0_FCR				address(0xFFFFFF64)
#define EFC0_FSR				address(0xFFFFFF68)
#define EFC1_FMR				address(0xFFFFFF70)
#define EFC1_FCR				address(0xFFFFFF74)
#define EFC1_FSR				address(0xFFFFFF78)
#define EFC_KEY					0x5A
#define EFC_FCMD_WP				0x01
#define EFC_FCMD_SLB			0x02
#define EFC_FCMD_WPL			0x03
#define EFC_FCMD_CLB			0x04
#define EFC_FCMD_EA				0x08
#define EFC_FCMD_SGPB			0x0B
#define EFC_FCMD_CGPB			0x0D
#define EFC_FCMD_SSB			0x0F
#define SAM_PAGE_SIZE			128

void sam_ba_write_word(void *destination, uint32_t word) {
	char *buffer = (char *) malloc(sizeof(char) * 20);
	sprintf(buffer, "W%08X,%08X#", (uint32_t)(destination), word);
	usart.push(buffer, (uint32_t)(strlen(buffer)));
	free(buffer);
}

void sam_ba_write_fcr0(uint8_t fcmd, uint32_t pagen) {
	sam_ba_write_word(EFC0_FCR, ((EFC_KEY << 24) | (pagen << 8) | fcmd));
}

void sam_ba_disable_auto_erase() {

	/* Will want to implement read word here in the future so as to be sure we don't overwrite any important stuff. */
	sam_ba_write_word(EFC0_FMR, 0x340180);

}

void sam_ba_jump_to_address(void *address) {
	char *buffer = (char *) malloc(sizeof(char) * 11);
	sprintf(buffer, "G%08X#", (uint32_t)(address));
	usart.push(buffer, (uint32_t)(strlen(buffer)));
	free(buffer);
}

size_t page_count_for_file_with_path(const char *path) {

	/* Open the file. */
	FILE *file = fopen (path, "rb");
	if (file == NULL) { printf("The file being opened, '%s', does not exist.\n", path); exit(EXIT_FAILURE); }

	/* Determine the size of the file. */
	fseek(file, 0L, SEEK_END);
	size_t size = ftell(file);
	fseek(file, 0L, SEEK_SET);
	fclose(file);

	return ceiling(size, SAM_PAGE_SIZE);

}

void *page_data_from_file_with_path(const char *path) {

	size_t pages = page_count_for_file_with_path(path);

	/* Open the file. */
	FILE *file = fopen (path, "rb");
	fseek(file, 0L, SEEK_SET);
	uint8_t *raw = (uint8_t *) malloc(pages * 128);

	for (int i = 0; i < pages; i ++) {
		for (int j = 0; j < 128; j ++) {
			uint8_t c = fgetc(file);
			if (!feof(file)) raw[((i * 128) + j)] = c; else raw[((i * 128) + j)] = 0;
		}

	}

	fclose(file);

	return raw;

}

void sam_ba_copy_chunk(void *destination, void *source) {

	/* Concatenate the address and the size of the file to be sent. */
	char *buffer = (char *) malloc(sizeof(char) * 20);
	sprintf(buffer, "S%08X,%08X#", (uint32_t)(destination), 128);
	usart.push(buffer, (uint32_t)(strlen(buffer)));
	free(buffer);

	/* Send the header. */
	usart.push((char []){ 0x01, 0x01, 0xFE }, 3);

	/* Send the data. */
	for (int i = 0; i < 128; i += 16) {
		usart.push((void *)(source + i), 16);
	}

	/* Send the checksum. */
	uint16_t cs = checksum(source, 128);
	usart.put(hi(cs));
	usart.put(lo(cs));

	/* Send termination. */
	usart.put(0x04);

}

void sam_erase_flash(void) {
	sam.format(); printf("Erasing flash memory.\n\n"); wait_with_progress(2); printf("\nDone.\n\n");
}

int sam_load_firmware(char *firmware) {

	led.rgb(8, 0, 0);
	printf("Flashing '%s'.\n\n", firmware);

	/* First, we need to put the device into programming mode. Let the user know, then throw up a progress bar. */
	printf("Entering programming mode.\n\n");

    sam.dfu();

    wait_with_progress(2);

	/* Next, we need to verify that the device has properly entered programming mode. Send the handshake sequence. */
	uint8_t connected = false;

    for (int i = 0; (i < 100) && !connected; i ++) { usart.push((uint8_t []){ 0x80, 0x80, 0x23 }, 3); uint8_t exp[3] = { 0x0A, 0x0D, 0x3E }; uint8_t res[3]; usart.pull(res, 3); connected = !memcmp(exp, res, 3); }

	if (!connected) {
		printf("\nERROR. Did not receive adknowledgement from the DFU.\n\n");
		return 1;
	}

	printf("\nReceived adknowledgement from the DFU.\n");

	/* If we get here, it means that we've successfuly connected to the 7S. Send the initialization sequence. */
	usart.push("N#", 2);

	/* We now need to disable automatic page erasure, because we want to erase flash entirely before programming in a new operating system. */
	sam_ba_disable_auto_erase();
	printf("\nErasing flash memory. ");

	/* Erase all internal flash. */
	sam_ba_write_fcr0(EFC_FCMD_EA, 0);
	char *sdk_path = getenv("FLIPPERSDK");
	if (!strlen(sdk_path)) { printf("FATAL ERROR. No environment variables declared for the Flipper SDK. Please reinstall the SDK and try again.\n\n"); exit(EXIT_FAILURE); }

	char *applet_path = strcat(sdk_path, "/resources/copy.applet");

	/* Next, we can move the copy applet into userspace. Since we can't directly access flash space through the SAM-BA, we need to have this code do it for us. */
	uint8_t applet[] = { 0x09, 0x48, 0x0A, 0x49, 0x0A, 0x4A, 0x02, 0xE0, 0x08, 0xC9, 0x08, 0xC0, 0x01, 0x3A, 0x00, 0x2A, 0xFA, 0xD1, 0x04, 0x48, 0x00, 0x28, 0x01, 0xD1, 0x01, 0x48, 0x85, 0x46, 0x70, 0x47, 0xC0, 0x46, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 };
	sam_ba_copy_chunk(USERSPACE, applet);

	/* Set the number of words we would like to transfer per call to the applet. */
	sam_ba_write_word(_APPLET_WORDS, 0x20);

	/* Set the applet's stack address. */
	sam_ba_write_word(_APPLET_STACK, (uint32_t)(SAM_STACK));

	/* Now that the applet is configured, we can send the actual file. */
	size_t pages = page_count_for_file_with_path(firmware);
	void *os = page_data_from_file_with_path(firmware);
	printf("Done.\n\nStarting the programming process. Writing page ");

	for (int i = 0; i < pages; i ++) {
		char buf[32];
		sprintf(buf, "%i / %zu.", i + 1, pages);
		printf("%s", buf);
		fflush(stdout);
		/* Copy the chunk into RAM as a buffer. */
		sam_ba_copy_chunk(USERSPACE + APPLET_SIZE, (os + (i * 128)));
		//printf("Copied chunk %i of %zi\n", i, pages);
		/* Set the destination address in the applet to be flash. */
		sam_ba_write_word(_APPLET_DESTINATION, (uint32_t)(FLASHSPACE + (i * 128)));
		//printf("Wrote dest word\n");
		/* Set the source address in the applet to be the buffer. */
		sam_ba_write_word(_APPLET_SOURCE, (uint32_t)(USERSPACE + APPLET_SIZE));
		//printf("Wrote source word\n");
		/* Launch the applet to move the data into the embedded flash controller. */
		sam_ba_jump_to_address(USERSPACE + APPLET_OFFSET);
		//printf("Wrote jump\n");
		/* Call the embedded flash controller to write the page into memory. */
		sam_ba_write_fcr0(EFC_FCMD_WP, i);
		//printf("Wrote fcr0\n");
		for (int j = 0; j < strlen(buf); j ++) printf("\b");
		fflush(stdout);
	}

	printf("\n\n");
	free(os);
	/* Reset the 7S so that the code is executed. */
	sam.power(false);
	printf("Resetting the device.\n");
	sam.power(true);
	led.rgb(0, 25, 0);
	/* ~ Turn on the USART interrupt. ~ */
//	usart.disable();
	/* Disconnect Flipper. */
	usb.disable();
	printf("\nProgramming complete.\n\n");
    return 0;
}


