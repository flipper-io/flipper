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

	/* capture interrupt signal */
	signal(SIGINT, sigint);

	struct libusb_context *context = NULL;
	struct libusb_device_handle *handle = NULL;
	int e;
	int len;
	uint8_t buf[DEBUG_IN_SIZE];

	e = libusb_init(&context);
	lf_assert(e == 0, E_LIBUSB, "Failed to initialize libusb.");

	handle = libusb_open_device_with_vid_pid(context, VENDOR, PRODUCT);
	lf_assert(handle, E_LIBUSB, "Failed to open the device.");

	/* Configure USB device. */
	e = libusb_claim_interface(handle, DEBUG_INTERFACE);
	lf_assert(e == 0, E_LIBUSB, "Failed to claim the debug interface.");

	while (alive) {

		uint8_t *_buf = buf;

		e = libusb_interrupt_transfer(handle, DEBUG_IN_ENDPOINT, buf, DEBUG_IN_SIZE, &len, 0);
		lf_assert(e == 0, E_LIBUSB, "Failed to complete transfer.");

		while (len --) printf("%c", *(uint8_t *)_buf ++);
	}

fail:
	libusb_close(handle);
	libusb_exit(context);

	return 0;

}
