#define __private_include__

#include <drivers/usb.h>

#include <fmr/fmr.h>

const struct _target host = {
	
	host_configure,
	
	host_invoke,
	
	host_push,
	
	host_pull
	
};

void host_configure(void) {
	
	/* ~ Configure USB as the host's communication protocol. ~ */
	
	((struct _target *)(&device)) -> bus = &usb;
	
}

uint32_t host_invoke(uint8_t module, uint8_t index, uint8_t argc, ...) {
	
	return 0;
	
}

uint32_t host_push(uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {
	
	return 0;
	
}

void host_pull(uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	
	
}