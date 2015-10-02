#define __private_include__

#include <fmr/fmr.h>

void self_configure(const struct _bus *bus) {
	
	
	
}

uint32_t fmr_call(void *function, uint8_t argc, void *argv) {
	
	return 0;
	
}

uint32_t self_invoke(const struct _target *sender) {
	
	verbose(" self -> invoke\n\n");
	
	/* ~ Dereference a pointer to the targeted object. ~ */
	
	//void *object = (void *)(objects[fmrpacket.destination.object]);
	
	/* ~ Dereference a pointer to the targeted function. ~ */
	
	//void *function = ((void **)(object))[fmrpacket.destination.index];
	
	/* ~ Invoke the targeted function with the appropriate arguments. ~ */
	
	return 0;//((uint32_t (*)(void))(function))();
	
}

uint32_t self_push(uint8_t object, uint8_t index, uint8_t argc, uint32_t length) {
	
	return 0;
	
}

void self_pull(uint8_t object, uint8_t index, uint8_t argc, uint32_t length) {
	
	
	
}