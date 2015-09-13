#define __private_include__

#include <drivers/usb.h>

void usb_configure(uint16_t configuration) {
	
	uint8_t devices = hid_enumerate(1, CARBON_VENDOR_ID, CARBON_PRODUCT_ID, CARBON_USAGE_PAGE, CARBON_USAGE);
	
	if (!devices) {
		
		printf("No device appears to be connected to this computer.\n\nPlease ensure that Flipper is properly connected and try again.\n\n");
		
		exit(EXIT_FAILURE);
		
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
	
	for (unsigned i = 0; i < length; i ++) { if (i % 8 == 0) printf("\n"); printf("%02X ", ((uint8_t *)(source))[i]); }
	
	printf("\n");
	
	hid_transmit_packet(source);
	
}

void usb_pull(void *destination, uint32_t length) {
	
	uint8_t buffer[32];
	
	hid_receive_packet(buffer);
	
	memcpy(destination, buffer, length);
	
}
