#define __private_include__

#include <fmr/fmr.h>

#include <platform/atmega.h>

void self_configure(const struct _bus *bus) {
	
	
	
}

uint32_t self_call(void) {
	
	/* ~ Dereference a pointer to the recipient object. ~ */
	
	void *object = (void *)(pgm_read_word(&objects[fmrpacket.recipient.object]));
	
	/* ~ Dereference a pointer to the recipient function. ~ */
	
	void *function = ((void **)(object))[fmrpacket.recipient.index];
	
	/* ~ Invoke the recipient function with the appropriate arguments. ~ */
	
	fmr_call(function, fmrpacket.recipient.argc, fmrpacket.body);
	
	return *(uint32_t *)(fmrpacket.body);
	
}