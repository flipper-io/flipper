#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

const void * const objects[] = { &self, &host, &device, &button, &at45, &fs, &i2c, &io, &led, &pwm, &sam, &spi, &timer, &usart, &usart1, &dbgu, &usb, &wifi, &fdl, &fmr };

void fmr_configure(void) {
	
	
	
}

void fmr_bind(fmr_handle *handle, uint16_t id) {
	
	*(uint32_t *)(handle) = id;
	
}

extern uint8_t fmr_padding;

#define FMR_INVOKE_OFFSET 6 * 2

uint32_t fmr_invoke(fmr_handle handle, uint8_t index, uint8_t argc, ...) {
	
	/* ~ Construct a va_list to access variadic arguments. ~ */
	
	va_list argv;
	
	/* ~ Initialize the va_list that we created above. ~ */
	
	va_start(argv, argc);
	
	/* ~ Load the variadic arguments into the outgoing packet. ~ */
	
	for (unsigned i = 0; i < (argc * 2); i += 2) {
		
		/* ~ Unstage an argument from the variadic argument list. ~ */
		
		unsigned arg = va_arg(argv, unsigned);
		
		/* ~ Load it into the packet. ~ */
		
		fmrpacket.body[i + FMR_INVOKE_OFFSET] = hi(arg); fmrpacket.body[i + FMR_INVOKE_OFFSET + 1] = lo(arg);
		
	}
	
	va_end(argv);
	
#pragma message("It would be better to find a way to implement the fmr_padding to not be global.")
	
	fmr_padding = argc * 2;
	
	/* ~ Call the remote function and return. ~ */
	
	return host.invoke(_fmr, _fmr_invoke, 6, little(lo16(handle)), little(hi16(handle)), little(index), 0, little(argc), 0);
	
}

void *fmr_resolve(void *source, uint32_t length) {
	
	return NULL;
	
}