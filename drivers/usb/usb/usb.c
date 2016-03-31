#define __private_include__
#include <flipper/usb.h>
#include <flipper/flipper.h>
#include <flipper/fmr.h>
#include <flipper/platform/platform.h>

extern int hid_enumerate(int max, int vid, int pid, int usage_page, int usage);
extern int8_t hid_transmit_packet(uint8_t device, uint8_t *buffer);
extern int8_t hid_receive_packet(uint8_t device, uint8_t *buffer);

void usb_configure() {

	uint8_t device = hid_enumerate(1, CARBON_VENDOR_ID, CARBON_PRODUCT_ID, CARBON_USAGE_PAGE, CARBON_USAGE);

	if (!device) {

		error.raise(E_FLIPPER_UNBOUND, ERROR_STRING(E_FLIPPER_UNBOUND_S));

	}

	flipper_device -> handle = 0;

}

void usb_enable(void) {

}

void usb_disable(void) {



}

bool usb_ready(void) {

	return 0;

}

void usb_put(uint8_t byte) {



}

uint8_t usb_get(void) {

	return 0;

}

void usb_push(void *source, uint32_t length) {

	/* ~ Ensure the push request can fit into a single packet. TODO: serialize packets ~ */
	if (length > FMR_PACKET_SIZE) { error.raise(E_TOO_BIG, ERROR_STRING("A request was made to send more information than can fit in a single USB packet.")); return; }

	/* ~ Send a usb packet to the active device. ~ */
	hid_transmit_packet((uint8_t)(flipper_device -> handle), source);

}

void usb_pull(void *destination, uint32_t length) {

	/* ~ Create a buffer into which an entire USB packet can be received. ~ */
	void *buffer = malloc(FMR_PACKET_SIZE);

	/* ~ Ensure the request for memory was granted. ~ */
	if (!buffer) error.raise(E_NO_MEM, ERROR_STRING(E_NO_MEM_S));

	/* ~ Receive a usb packet from the active device. ~ */
	hid_receive_packet((uint8_t)(flipper_device -> handle), buffer);

	/* ~ Copy the appropriate amount of information from the buffer to the destination. ~ */
	memcpy(destination, buffer, length);

	/* ~ Free the buffer. ~ */
	free(buffer);

}
