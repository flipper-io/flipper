#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

const void * const objects[] = { &self, &host, &device, &button, &at45, &fs, &i2c, &io, &led, &pwm, &sam, &spi, &timer, &usart, &usart1, &dbgu, &usb, &wifi, &fdl, &fmr };

void fmr_configure(void) {
	
	
	
}

void fmr_bind(fmr_handle *handle, uint16_t id) {
	
	*(uint32_t *)(handle) = id;
	
}

uint32_t fmr_invoke(fmr_handle handle, uint8_t index, uint8_t argc, ...) {
	
	return host.invoke(_fmr, _fmr_invoke, 6, little(lo16(handle)), little(hi16(handle)), little(index), 0, little(argc), 0);
	
}

void *fmr_resolve(void *source, uint32_t length) {
	
	return NULL;
	
}