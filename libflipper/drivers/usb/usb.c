#define __private_include__

#include <usb/usb.h>

#include <fmr/fmr.h>

void usb_configure(uint16_t configuration) {
	
	uint8_t devices = hid_enumerate(1, CARBON_VENDOR_ID, CARBON_PRODUCT_ID, CARBON_USAGE_PAGE, CARBON_USAGE);
	
	if (!devices) {
		
		printf("No device appears to be connected to this computer.\n\nPlease ensure that Flipper is properly connected and try again.\n\n");
		
		exit(EXIT_FAILURE);
		
	}
	
	else {
		
		printf("Successfully connected to Flipper device over USB.\n\n");
		
	}
	
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
	
	/* ~ Allocate a buffer to store a USB packet. ~ */
	
	void *packet = malloc(USB_PACKET_LENGTH);
	
	/* ~ Clear the buffer. ~ */
	
	memset(packet, 0x00, USB_PACKET_LENGTH);
	
	/* ~ Copy the data into the buffer. ~ */
	
	memcpy(packet, source, length);
	
	/* ~ Send the packet. ~ */
	
	hid_transmit_packet(packet);
	
}

void usb_pull(void *destination, uint32_t length) {
	
	uint8_t buffer[32];
	
	hid_receive_packet(buffer);
	
	memcpy(destination, buffer, length);
	
}
