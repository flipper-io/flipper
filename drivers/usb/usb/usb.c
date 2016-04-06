#define __private_include__
#include <flipper/usb.h>
#include <flipper/flipper.h>
#include <flipper/fmr.h>
#include <flipper/platform/platform.h>

int hid_enumerate(int max, int vid, int pid, int usage_page, int usage);
int8_t hid_transmit_packet(uint8_t device, uint8_t *buffer);
int8_t hid_receive_packet(uint8_t device, uint8_t *buffer);

void usb_configure() {

	uint8_t devices = hid_enumerate(1, CARBON_VENDOR_ID, CARBON_PRODUCT_ID, CARBON_USAGE_PAGE, CARBON_USAGE);

	if (!devices) {
		error_raise(E_FLIPPER_UNBOUND, "No device appears to be connected to this computer.\nPlease ensure that Flipper is properly connected and try again.\n");
	}

	for (uintptr_t i = 0; i < devices; i ++) {
		flipper_device -> handle = (void *)(i);
		if (config.read(CONFIG_NAME) == flipper_device -> identifier) return;
	}

	error_raise(E_FLIPPER_NOT_FOUND, "Could not find device named '%s'.\n", flipper_device -> name);

}

void usb_enable(void) {

}

void usb_disable(void) {



}

uint8_t usb_ready(void) {

	return 0;

}

void usb_put(uint8_t byte) {



}

uint8_t usb_get(void) {

	return 0;

}

void usb_push(void *source, uint32_t length) {

	/* ~ Ensure the push request can fit into a single packet. TODO: serialize packets ~ */
	if (length > FMR_PACKET_SIZE) { error_raise(E_TOO_BIG, ""); return; }

	/* ~ Send a usb packet to the active device. ~ */
	hid_transmit_packet((uint8_t)(flipper_device -> handle), source);

}

void usb_pull(void *destination, uint32_t length) {

	/* ~ Create a buffer into which an entire USB packet can be received. ~ */
	void *buffer = malloc(FMR_PACKET_SIZE);

	/* ~ Ensure the request for memory was granted. ~ */
	if (!buffer) error_raise(E_NO_MEM, "");

	/* ~ Receive a usb packet from the active device. ~ */
	hid_receive_packet((uint8_t)(flipper_device -> handle), buffer);

	/* ~ Copy the appropriate amount of information from the buffer to the destination. ~ */
	memcpy(destination, buffer, length);

	/* ~ Free the buffer. ~ */
	free(buffer);

}
