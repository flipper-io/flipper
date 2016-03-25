#define __private_include__
#include <usb/usb.h>
#include <platform.h>
#include <usb/usb.h>
#include <fmr/fmr.h>

void usb_configure() {

#ifndef __disable_usb__

	uint8_t devices = hid_enumerate(1, CARBON_VENDOR_ID, CARBON_PRODUCT_ID, CARBON_USAGE_PAGE, CARBON_USAGE);

	if (!devices) {
		error.raise(E_FLIPPER_UNBOUND, ERROR_STRING(E_FLIPPER_UNBOUND_S));
	}

#endif

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

#ifndef __disable_usb__

	/* ~ Allocate a buffer to store a USB packet. ~ */

	void *packet = malloc(FMR_PACKET_SIZE);

	if(!packet) {
		error.raise(E_NO_MEM, ERROR_STRING(E_NO_MEM_S));
		return;
	}

	/* ~ Clear the buffer. ~ */

	memset(packet, 0x00, FMR_PACKET_SIZE);

	/* ~ Copy the data into the buffer. ~ */

	memcpy(packet, source, length);

	/* ~ Send the packet. ~ */

	hid_transmit_packet(packet);

	free(packet);

#endif

}

void usb_pull(void *destination, uint32_t length) {

#ifndef __disable_usb__

	uint8_t *buffer = (uint8_t *)(malloc(sizeof(uint8_t) * FMR_PACKET_SIZE));

	if(!buffer) {
		error.raise(E_NO_MEM, ERROR_STRING(E_NO_MEM_S));
	}

	hid_receive_packet(buffer);

	memcpy(destination, buffer, length);

	free(buffer);

#endif

}
