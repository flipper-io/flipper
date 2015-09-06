#define __private_include__

#include <bus/bus.h>

#include <fmr/fmr.h>

uint32_t target_invoke(const struct _target *target, uint8_t module, uint8_t index, uint8_t argc, va_list *argv) {
	
	/* ~ Multiply the argument count by two. ~ */
	
	argc *= 4;
	
	/* ~ Populate the message body with the information needed to make a remote procedure call. ~ */
	
	fmr_buffer[0] = module;
	
	fmr_buffer[1] = index;
	
	fmr_buffer[2] = argc;
	
	for (unsigned i = 0; i < argc; i += 2) {
		
		unsigned arg = va_arg(*argv, unsigned);
		
		fmr_buffer[i + 3] = hi(arg); fmr_buffer[i + 4] = lo(arg);
		
	}
	
	/* ~ Release the variadic argument list. ~ */
	
	va_end(*argv);
	
	/* ~ Push the message to the device. ~ */
	
	target -> bus -> push(fmr_buffer, sizeof(fmr_buffer));
	
	return 0;
	
}

uint32_t target_push(const struct _target *target, uint8_t _module, uint8_t _index, uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, va_list *argv) {
	
	return 0;
	
}

void target_pull(const struct _target *target, uint8_t _module, uint8_t _index, uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, va_list *argv) {
	
	
	
}