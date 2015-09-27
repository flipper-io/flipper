#define __private_include__

#include <flash/flash.h>

#include <fmr/fmr.h>

void flash_configure(void) {

	
	
}

void flash_enable(void) {
	
	host.invoke(_flash, _flash_enable, NO_ARGS);
	
}

void flash_disable(void) {
	
	host.invoke(_flash, _flash_disable, NO_ARGS);
	
}

void flash_reset(void) {
	
	host.invoke(_flash, _flash_reset, NO_ARGS);
	
}

/*
 
 Note that flash_alloc() and flash_free() are defined in the common 'alloc.c' for organizational purposes.
 
*/

void flash_format(void) {
	
	host.invoke(_flash, _flash_format, NO_ARGS);
	
}

void flash_push(void *source, uint32_t length, fsp destination) {
	
	host.push(_flash, _flash_push, 2, source, length, hi16(destination), lo16(destination));
	
}

void flash_pull(void *destination, uint32_t length, fsp source) {
	
	host.pull(_flash, _flash_pull, 2, destination, length, hi16(source), lo16(source));
	
}

void *flash_dereference(fsp source, uint32_t length) {
	
	void *local = malloc(length);
	
	flash_pull(local, length, source);
	
	return local;
	
}