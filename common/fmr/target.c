#define __private_include__

#include <fmr/bus.h>

#include <fs/crc.h>

#include <fmr/fmr.h>

uint32_t target_invoke(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, va_list *argv) {
	
	if (!(target -> bus)) { error("Error. No device is attached to this instance of libflipper. Use flipper.attach(\"device name\", FLIPPER_SOURCE_USB) to connect over USB.\n\n"); return  0; }
	
	/* ~ Multiply the argument count by two. ~ */
	
	argc *= sizeof(uint16_t);
	
	struct _fmr_header *header = &fmrpacket.header;

	struct _fmr_destination *destination = &fmrpacket.destination;
	
	uint8_t *body = fmrpacket.body;
	
	/* ~ Allocate memory to store a packet. ~ */
	
	header -> fe = 0xFE;
	
	/* ~ Populate the message body with the information needed to make a remote procedure call. ~ */
	
	header -> length = sizeof(struct _fmr_destination) + argc;
	
	/* ~ Populate the packet's destination. ~ */
	
	destination -> object = object;
	
	destination -> index = index;
	
	destination -> argc = argc;
	
	/* ~ Populate the packet's body. ~ */
	
	for (unsigned i = 0; i < argc; i += 2) {
		
		unsigned arg = va_arg(*argv, unsigned);
		
		body[i] = hi(arg); body[i + 1] = lo(arg);
		
	}
	
	/* ~ Release the variadic argument list. ~ */
	
	va_end(*argv);
	
	/* ~ Generate a checksum. ~ */
	
	header -> checksum = checksum((void *)(&(destination -> object)), sizeof(struct _fmr_destination) + argc);
	
	/* ~ Push the message to the device. ~ */
	
	target -> bus -> push(&fmrpacket, sizeof(struct _fmr_header) + sizeof(struct _fmr_destination) + argc);
	
	/* ~ Use a local variable to store the return value. ~ */
	
	uint32_t retval;
	
	/* ~ Retrieve the return value from the device. ~ */
	
	target -> bus -> pull(&retval, sizeof(uint32_t));
	
	/* ~ Give the return value back to the parent function. ~ */
	
	return retval;
	
}

uint32_t target_push(const struct _target *target, uint8_t _object, uint8_t _index, uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, va_list *argv) {
	
	
	
	return 0;
	
}

void target_pull(const struct _target *target, uint8_t _object, uint8_t _index, uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, va_list *argv) {
	
	
	
}