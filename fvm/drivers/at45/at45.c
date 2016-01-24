#define __private_include__

#include <at45/at45.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

void *virtual_at45;

void at45_configure(void) {

	virtual_at45 = malloc(sizeof(uint8_t) * 1024 * 1024 * 2);
	
}

void at45_enable(void) {
	
	
	
}

void at45_disable(void) {
	
	
	
}

void at45_reset(void) {
	
	
	
}

void at45_format(void) {
	
	
	
}

void at45_push(void *source, uint32_t length, fsp destination) {
	
	memcpy((void *)(virtual_at45 + destination), source, length);
	
}

void at45_pull(void *destination, uint32_t length, fsp source) {
	
	memcpy(destination, (void *)(virtual_at45 + source), length);
	
}

void *at45_dereference(fsp source, uint32_t length) {
	
	void *region = (void *) malloc(length);
	
	memcpy(region, (void *)(virtual_at45 + source), length);
	
	return region;
	
}