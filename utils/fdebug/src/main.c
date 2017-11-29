#include <signal.h>
#include <stdio.h>
#include <libusb-1.0/libusb.h>

#define VENDOR 0x16C0
#define PRODUCT 0x0480
#define DEBUG_INTERFACE 1
#define DEBUG_BUFFER_SIZE 32
#define DEBUG_IN_ENDPOINT 0x03 | 0x80

static volatile int alive = 1;

void sigint(int signal) {
	alive = 0;
}

int main(int argc, char *argv[]) {

	signal(SIGINT, sigint);

	struct libusb_context *context;

	int _e = libusb_init(&context);
	if (_e) {
		fprintf(stderr, "Failed to initialize libusb.\n");
	}

	libusb_set_debug(context, LIBUSB_LOG_LEVEL_ERROR);

	struct libusb_device_handle *handle = libusb_open_device_with_vid_pid(context, VENDOR, PRODUCT);
	if (!handle) {
		fprintf(stderr, "Failed to open the device.\n");
		goto exit;
	}

	/* Configure USB device. */
	_e = libusb_claim_interface(handle, DEBUG_INTERFACE);
	if (_e) {
		fprintf(stderr, "Failed to claim the debug interface.\n");
		goto exit;
	}

	_e = libusb_set_configuration(handle, 1);
	if (_e) {
		fprintf(stderr, "Failed to configure the device.\n");
		goto exit;
	}

	int len;

	unsigned char incoming[DEBUG_BUFFER_SIZE];

	while (alive) {
		_e = libusb_interrupt_transfer(handle, DEBUG_IN_ENDPOINT, incoming, sizeof(incoming), &len, 0);
		if (_e) {
			fprintf(stderr, "Something went wronge with the transfer.\n");
		}
		printf("%s\n", incoming);
	}

exit:

	if (handle) libusb_close(handle);
	if (context) libusb_exit(context);

	return 0;

}
