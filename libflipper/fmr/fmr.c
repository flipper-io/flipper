#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

const void * const objects[] = { &button, &flash, &self, &host, &device, &fs, &i2c, &io, &led, &pwm, &sam, &spi, &timer, &usart, &usart1, &dbgu, &usb, &wifi };

uint32_t internal_call(void *function, uint8_t argc, void *argv) {
	
	return ((uint32_t (*)(void)) function)();
	
}