#define __private_include__

#include <fmr/fmr.h>

const struct _target self = {
	
	self_configure,
	
	self_invoke,
		
	self_push,
	
	self_pull
	
};

void self_configure(void) {
	
	
	
}

uint32_t self_invoke(uint8_t module, uint8_t index, uint8_t argc, ...) {
	
	return 0;
	
}

uint32_t self_push(uint8_t module, uint8_t index, uint8_t argc, void *source, uint32_t length, ...) {
	
	return 0;
	
}

void self_pull(uint8_t module, uint8_t index, uint8_t argc, void *destination, uint32_t length, ...) {
	
	
	
}