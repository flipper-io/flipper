#include <stdio.h>
#define __private_include__
#include <flipper.h>
#include <platform/posix.h>
#include <platform/fvm.h>
#include <platform/atsam4s16b.h>

#define v(i) atoi(argv[i])

#define SOH 0x01
#define EOT 0x04
#define ACK 0x06
#define NAK 0x15
#define ETB 0x17
#define CAN 0x18
#define XLEN 128

struct __attribute__((__packed__)) _xpacket {
	uint8_t header;
	uint8_t number;
	uint8_t _number;
	uint8_t data[XLEN];
	uint16_t checksum;
};

uint8_t applet[] = {
	0x09, 0x48, 0x0A, 0x49, 0x0A, 0x4A, 0x02, 0xE0,
	0x08, 0xC9, 0x08, 0xC0, 0x01, 0x3A, 0x00, 0x2A,
	0xFA, 0xD1, 0x04, 0x48, 0x00, 0x28, 0x01, 0xD1,
	0x01, 0x48, 0x85, 0x46, 0x70, 0x47, 0x00, 0xBF,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00
};

void sam_ba_send_command(char command, uint32_t lvar, uint32_t rvar) {
	char buffer[20];
	sprintf(buffer, "%c%08X,%08X#", command, lvar, rvar);
	uart.push(buffer, sizeof(buffer) - 1);
}

void sam_ba_write_word(void *destination, uint32_t word) {
	sam_ba_send_command('W', (uint32_t)destination, word);
}

int sam_ba_copy(void *destination, void *source, uint32_t length) {
	sam_ba_send_command('S', (uint32_t)destination, length);

	/* Calculate the number of packets needed to perform the transfer. */
	int packets = lf_ceiling(length, XLEN);
	uint8_t pno = 1;
	void *_source = source;
	uint8_t retries = 0;
	while (packets --) {
		uint32_t _len = XLEN;
		if (length < _len) {
			_len = length;
		}
		/* Construct the base packet. */
		struct _xpacket packet = { SOH, pno, ~pno, {0}, 0x00 };
		/* Copy the data into the packet. */
		memcpy(packet.data, _source + ((pno - 1) * XLEN), _len);
		/* Perform the checksum. */
		packet.checksum = lf_checksum(packet.data, sizeof(packet.data));
send:
		/* Transfer the packet. */
		uart.push(&packet, sizeof(struct _xpacket));
		char c = uart.get();
		if (c != ACK && retries < 4) {
			printf("Got 0x%02x\n", c);
			retries ++;
			goto send;
		} else {
			error_raise(E_ACK, error_message("Did not receive acknowledgement from device."));
			return lf_error;
		}
		/* Increment the source. */
		_source += XLEN;
		/* Decrement the length. */
		length -= _len;
		/* Increment the packet number. */
		pno ++;
	}
	return lf_success;
}

int main(int argc, char *argv[]) {

	//flipper_attach_endpoint("fvm", &lf_fvm_ep);
	flipper.attach();
	led.rgb(0, 0, 0);
	/* Enter DFU mode. */
	cpu.dfu();
	for (int i = 0; i < 3; i ++) {
		uart.put('#');
		char ack[3];
		uart.pull(ack, sizeof(ack));
		if (!memcmp(ack, (char []){ 0x0a, 0x0d, 0x3e }, sizeof(ack))) {
			fprintf(stderr, "Successfully entered update mode.\n");
			goto connected;
		}
		printf("0x%02x 0x%02x 0x%02x", ack[0], ack[1], ack[2]);
	}
	fprintf(stderr, KRED "Failed to enter update mode.\n");
	return EXIT_FAILURE;
connected:
	printf("Internal ROM space starts at 0x%08x.", IRAM_ADDR);

	int _e = sam_ba_copy((void *)IRAM_ADDR, applet, sizeof(applet));
	if (_e < lf_success) {
		return EXIT_FAILURE;
	}
	printf("Successfully uploaded applet.");

    return EXIT_SUCCESS;
}

// printf("\n");
// for (int i = 0; i < length; i ++) {
// 	printf("%i: 0x%02x (%c)\n", i, *(uint8_t *)(source + i), *(uint8_t *)(source + i));
// }
// printf("---");
