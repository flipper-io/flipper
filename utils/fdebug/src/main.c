#include <flipper.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <libusb.h>

#define VENDOR 0x16C0
#define PRODUCT 0x0480
#define DEBUG_INTERFACE 1
#define DEBUG_BUFFER_SIZE 32
#define DEBUG_IN_ENDPOINT 0x03 | 0x80
/* Timeout here must be larger than the flush rate of the device. */
#define DEBUG_TIMEOUT 0

static volatile int alive = 1;

void sigint(int signal) {
	alive = 0;
}

void print_time(void) {
	time_t rawtime;
	struct tm * timeinfo;
	time(&rawtime);
	timeinfo = localtime(&rawtime);
	printf("%02d:%02d:%02d ", timeinfo->tm_hour, timeinfo->tm_min, timeinfo->tm_sec);
}

int main(int argc, char *argv[]) {

	signal(SIGINT, sigint);

	struct libusb_context *context;

	int e = libusb_init(&context);
	if (e) {
		fprintf(stderr, "Failed to initialize libusb.\n");
	}

	struct libusb_device_handle *handle = libusb_open_device_with_vid_pid(context, VENDOR, PRODUCT);
	if (!handle) {
		fprintf(stderr, "Failed to open the device.\n");
		goto exit;
	}

	/* Configure USB device. */
	e = libusb_claim_interface(handle, DEBUG_INTERFACE);
	if (e) {
		fprintf(stderr, "Failed to claim the debug interface.\n");
		goto exit;
	}

	int len;

	uint8_t buf[DEBUG_IN_SIZE];

	while (alive) {
		e = libusb_interrupt_transfer(handle, DEBUG_IN_ENDPOINT, buf, DEBUG_IN_SIZE, &len, 0);
		if (e == 0) {
			if (len > 0) {
				uint8_t *_buf = buf;
				while (len --) printf("%c", *(uint8_t *)(_buf ++));
				fflush(stdout);
			}
		} else if (e == LIBUSB_ERROR_TIMEOUT) {

		} else {
			fprintf(stderr, "Something went wrong with the transfer.\n");
			goto exit;
		}
	}

exit:

	if (handle) libusb_close(handle);
	if (context) libusb_exit(context);

	return 0;

}
