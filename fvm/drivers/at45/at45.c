#define __private_include__

#include <flash/flash.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <platform/fmr.h>

void *virtual_flash;

void flash_configure(void) {

	virtual_flash = malloc(sizeof(uint8_t) * 1024 * 1024 * 2);
	
}

void flash_enable(void) {
	
	
	
}

void flash_disable(void) {
	
	
	
}

void flash_reset(void) {
	
	
	
}

void flash_format(void) {
	
	
	
}

void flash_push(void *source, uint32_t length, fsp destination) {
	
	memcpy((void *)(virtual_flash + destination), source, length);
	
}

void flash_pull(void *destination, uint32_t length, fsp source) {
	
	memcpy(destination, (void *)(virtual_flash + source), length);
	
}

void *flash_dereference(fsp source, uint32_t length) {
	
	void *region = (void *) malloc(length);
	
	memcpy(region, (void *)(virtual_flash + source), length);
	
	return region;
	
}