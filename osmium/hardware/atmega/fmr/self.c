#define __private_include__

#include <fmr/fmr.h>

#include <usart/usart.h>

#include <led/led.h>

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
	
	/* ~ Dereference a pointer to the targeted object. ~ */
	
	void *object = (void *)(pgm_read_word(&objects[fmrpacket.object]));
	
	/* ~ Dereference a pointer to the targeted function. ~ */
	
	void *function = ((void **)(object))[fmrpacket.index];
	
	/* ~ Save a copy of the argument count. ~ */
	
	uint8_t argc = fmrpacket.argc;
	
	/* ~ Invoke the targeted function with the appropriate arguments. ~ */
	
	fmr_call(function, argc, &fmrpacket.body);
	
	/* ~ Return whatever we received back to the device that sent us a message. ~ */
	
	//sender -> bus -> push(fmrpacket.body, sizeof(uint32_t));
	
	/* ~ There is no point in returning the retval here, as self_invoke is not implemented. ~ */
	
	return 0;
	
}

uint32_t self_push(uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {
	
	return 0;
	
}

void self_pull(uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	
	
}