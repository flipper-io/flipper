#define __private_include__

#include <flipper/flipper.h>

#include <platform/at91sam.h>

const void * const objects[] = { &host, &self, &device, &button, &at45, &fs, &i2c, &io, &led, &pwm, &sam, &spi, &timer, &usart, &usart1, &dbgu, &usb, &wifi, &fdl, &fmr };

void fmr_configure(void) {
	
	
	
}

void fmr_bind(fmr_handle *handle, uint16_t id) {
	
	
	
}

void test(uint32_t a, uint32_t b, uint32_t c, uint32_t d) {
	
	serprintf("Called with: 0x%08x, 0x%08x, 0x%08x, 0x%08x\n", a, b, c, d);
	
}

uint32_t fmr_invoke(fmr_handle handle, uint8_t index, uint8_t argc, ...) {
	
	/* ~ When we get here, we need to use the handler information to perform an internal call to a module loaded by FDL. ~ */
	
	void *module = fdl_load(handle);
	
	/* ~ Ensure the module was successfully loaded. ~ */
	
	if (!module) { error.raise(0); return -1; }
	
	void *configure = module + *(uint32_t *)(module);
	
	/* ~ Configure the module. ~ */
	
	((void (*)(void))(configure))();
	
	/* ~ Get the address of the function. ~ */
	
	void *function = module + *(uint32_t *)(module + (index * sizeof(uint32_t)));
	
	/* ~ The address of the variadic argument list is the address of the packet's body plus the 6 bytes needed for this function call. ~ */
	
	void *argv = fmrpacket.body + 6 * 2;
	
	/* ~ Call the module function and return what we get back. ~ */
	
	uint32_t retval = internal_call(function, argc, argv);
	
	return retval;
	
}

void *fmr_resolve(void *source, uint32_t length) {
	
	return NULL;
	
}