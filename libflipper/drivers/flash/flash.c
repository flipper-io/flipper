#define __private_include__

#include <flash/flash.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

void flash_configure(void) {

	
	
}

void flash_enable(void) {
	
	device.invoke(_flash, _flash_enable, NO_ARGS);
	
}

void flash_disable(void) {
	
	device.invoke(_flash, _flash_disable, NO_ARGS);
	
}

void flash_reset(void) {
	
	device.invoke(_flash, _flash_reset, NO_ARGS);
	
}

fsp flash_alloc(uint32_t length) {
	
	return device.invoke(_flash, _flash_alloc, 2, hi16(length), lo16(length));
	
}

void flash_free(fsp pointer) {
	
	device.invoke(_flash, _flash_free, 2, hi16(pointer), lo16(pointer));
	
}

void flash_format(void) {
	
	device.invoke(_flash, _flash_format, NO_ARGS);
	
}

void flash_push(void *source, uint32_t length, fsp destination) {
	
	device.push(_flash, _flash_push, 2, source, length, hi16(destination), lo16(destination));
	
}

void flash_pull(void *destination, uint32_t length, fsp source) {
	
	device.pull(_flash, _flash_pull, 2, destination, length, hi16(source), lo16(source));
	
}

void *flash_dereference(fsp source, uint32_t length) {
	
	void *local = malloc(length);
	
	flash_pull(local, length, source);
	
	return local;
	
}