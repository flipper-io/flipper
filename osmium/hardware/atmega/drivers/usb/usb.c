#define __private_include__

#include <drivers/usb.h>

#include <platform/hid.h>

void usb_configure(uint16_t configuration) {
	
	configure_usb();
	
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
	
	
	
}

void usb_pull(void *destination, uint32_t length) {
	
	
	
}
