#define __private_include__

#include <fmr/fmr.h>

#include <drivers/usart.h>

#include <drivers/button.h>

#include <platform/atmega.h>

const struct _self self = {
	
	self_configure,
	
	self_invoke,
		
	self_push,
	
	self_pull
	
};

void self_configure(const struct _bus *bus) {
	
	
	
}

uint32_t self_invoke(const struct _target *sender) {
	
	/* ~ Dereference a pointer to the targeted module. ~ */
	
	void *object = (void *)(pgm_read_word(&modules[fmr_buffer[0]]));
	
	/* ~ Dereference a pointer to the targeted function. ~ */
	
	void *function = ((void **)(object))[fmr_buffer[1]];
	
	/* ~ Save a copy of the argument count. ~ */
	
	uint8_t argc = fmr_buffer[2];
	
	/* Offset the message. */
	
	memmove(fmr_buffer, fmr_buffer + 3, FLIPPER_PACKET_SIZE - 3);
	
	/* ~ Invoke the targeted function with the appropriate arguments. ~ */
	
	fmr_call(function, argc, fmr_buffer);
	
	/* ~ Return whatever we received back to the device that sent us a message. ~ */
	
	sender -> bus -> push(fmr_buffer, sizeof(uint32_t));
	
	/* ~ There is no point in returning the retval here, as self_invoke is not implemented. ~ */
	
	return 0;
	
}

uint32_t self_push(uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {
	
	return 0;
	
}

void self_pull(uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	
	
}