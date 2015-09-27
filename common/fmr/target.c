#define __private_include__

#include <fmr/bus.h>

#include <fs/crc.h>

#include <fmr/fmr.h>

/* ~ Make it easier to access parts of the packet. ~ */

struct _fmr_header *header = &fmrpacket.header;

struct _fmr_recipient *recipient = &fmrpacket.recipient;

uint8_t *body = fmrpacket.body;

uint32_t target_invoke(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, va_list *argv) {
	
	if (!(target -> bus)) { error("Error. No device is attached to this instance of libflipper. Use flipper.attach(\"device name\", FLIPPER_SOURCE_USB) to connect over USB.\n\n"); return  0; }
	
	/* ~ Multiply the argument count by two. ~ */
	
	argc *= sizeof(uint16_t);
	
	header -> fe = 0xFE;
	
	/* ~ Populate the message body with the information needed to make a remote procedure call. ~ */
	
	header -> length = sizeof(struct _fmr_packet) - FMR_BODY_SIZE + argc;
	
	/* ~ Populate the packet's destination. ~ */
	
	recipient -> object = object;
	
	recipient -> index = index;
	
	recipient -> argc = argc;
	
	/* ~ Populate the packet's body. ~ */
	
	for (unsigned i = 0; i < argc; i += 2) {
		
		unsigned arg = va_arg(*argv, unsigned);
		
		body[i] = hi(arg); body[i + 1] = lo(arg);
		
	}
	
	/* ~ Release the variadic argument list. ~ */
	
	va_end(*argv);
	
	/* ~ Generate a checksum. ~ */
	
	header -> checksum = checksum((void *)(&(recipient -> object)), (header -> length) - sizeof(struct _fmr_header));
	
	/* ~ Push the message to the device. ~ */
	
	target -> bus -> push(&fmrpacket, sizeof(struct _fmr_header) + sizeof(struct _fmr_recipient) + argc);
	
	/* ~ Use a local variable to store the return value. ~ */
	
	uint32_t retval;
	
	/* ~ Retrieve the return value from the device. ~ */
	
	target -> bus -> pull(&retval, sizeof(uint32_t));
	
	/* ~ Give the return value back to the parent function. ~ */
	
	return retval;
	
}

uint32_t target_push(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, va_list *argv) {
	
	/* ~ The parameter 'argc' specifies the number of arguments the target function expects. Multiply it by the size of a uint16_t. ~ */
	
	argc = argc * sizeof(uint16_t);
	
	/* ~ Construct the header of the packet. ~ */
	
	header -> fe = 0xFE;
	
	/* ~ The length of the packet will be equal to a full packet plus the space required to make a nested invocation, as well as the argument count. This does not yet accout for the first data chunk. ~ */
	
	header -> length = sizeof(struct _fmr_packet) - FMR_BODY_SIZE + FMR_PUSH_PARAMETER_SIZE + argc;
	
	/* ~ Construct the packet's destination. This is the function that will be called on the device to handle receiving data. ~ */
	
	recipient -> object = _self;
	
	recipient -> index = _self_push;
	
	recipient -> argc = FMR_PUSH_PARAMETER_SIZE;
	
	/* ~ Now we need to construct the packet's body. This contains layered destination information regarding the target function that will be processed by the self's helper function. ~ */
	
	body[0] = hi(object);
	
	body[1] = lo(object);
	
	body[2] = hi(index);
	
	body[3] = lo(index);
	
	body[4] = hi(argc);
	
	body[5] = lo(argc);
	
	body[6] = hi(hi16(length));
	
	body[7] = lo(hi16(length));
	
	body[8] = hi(lo16(length));
	
	body[9] = lo(lo16(length));
	
	/* ~ We use this space in the packet to load the arguments just as we normally would. ~ */
	
	for (int i = 0; i < argc; i += 2) {
		
		unsigned arg = va_arg(*argv, unsigned);
		
		body[i + 10] = hi(arg); body[i + 11] = lo(arg);
		
	}
	
	/* -------------------------------------- ACTUALLY MOVE THE DATA -------------------------------------- */

	size_t remaining;
	
	char *current = (char *)(body + FMR_PUSH_PARAMETER_SIZE + argc);
	
out:
	
	/* ~ Okay, we've now consumed quite a bit of the maximum FLIPPER_DATAGRAM_SIZE. How much do we have left? ~ */
	
	remaining = FLIPPER_DATAGRAM_SIZE - (header -> length);
	
	/* ~ Now, let's use the space we have left to send some data! ~ */
	
	do { *(char *)(current ++) = *(char *)(source ++); (header -> length) ++; } while ((-- length) && (-- remaining));
	
	header -> checksum = checksum((void *)(&(recipient -> object)), (header -> length) - sizeof(struct _fmr_header));
	
	target -> bus -> push(&fmrpacket, header -> length);
	
	if (length) {
		
		/* ~ We need to send another packet! ~ */
		
		header -> length = sizeof(struct _fmr_header);
		
		current = (char *)(recipient);
		
		goto out;
		
	}
	
	/* ~ Use a local variable to store the return value. ~ */
	
	uint32_t retval;
	
	/* ~ Retrieve the return value from the device. ~ */
	
	target -> bus -> pull(&retval, sizeof(uint32_t));
	
	/* ~ Give the return value back to the parent function. ~ */
	
	return retval;
	
}

void target_pull(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, va_list *argv) {
	
	/* ~ The parameter 'argc' specifies the number of arguments the target function expects. Multiply it by the size of a uint16_t. ~ */
	
	argc = argc * sizeof(uint16_t);
	
	/* ~ Construct the header of the packet. ~ */
	
	header -> fe = 0xFE;
	
	/* ~ The length of the packet will be equal to a full packet plus the space required to make a nested invocation, as well as the argument count. This does not yet accout for the first data chunk. ~ */
	
	header -> length = sizeof(struct _fmr_packet) - FMR_BODY_SIZE + FMR_PUSH_PARAMETER_SIZE + argc;
	
	/* ~ Construct the packet's destination. This is the function that will be called on the device to handle receiving data. ~ */
	
	recipient -> object = _self;
	
	recipient -> index = _self_pull;
	
	recipient -> argc = FMR_PUSH_PARAMETER_SIZE;
	
	/* ~ Now we need to construct the packet's body. This contains layered destination information regarding the target function that will be processed by the self's helper function. ~ */
	
	body[0] = hi(object);
	
	body[1] = lo(object);
	
	body[2] = hi(index);
	
	body[3] = lo(index);
	
	body[4] = hi(argc);
	
	body[5] = lo(argc);
	
	body[6] = hi(hi16(length));
	
	body[7] = lo(hi16(length));
	
	body[8] = hi(lo16(length));
	
	body[9] = lo(lo16(length));
	
	/* ~ We use this space in the packet to load the arguments just as we normally would. ~ */
	
	for (int i = 0; i < argc; i += 2) {
		
		unsigned arg = va_arg(*argv, unsigned);
		
		body[i + 10] = hi(arg); body[i + 11] = lo(arg);
		
	}
	
	/* ~ Send the packet to initiate the pulling sequence. ~ */
	
	header -> checksum = checksum((void *)(&(recipient -> object)), (header -> length) - sizeof(struct _fmr_header));
	
	target -> bus -> push(&fmrpacket, header -> length);
	
	target -> bus -> pull(&fmrpacket, sizeof(fmrpacket));
	
	/* -------------------------------------- ACTUALLY GET THE DATA -------------------------------------- */
	
	uint32_t len = length;
	
	size_t remaining;
	
	char *current;
	
in:
	
	remaining = FLIPPER_DATAGRAM_SIZE - sizeof(struct _fmr_header);
	
	current = (char *)(&fmrpacket.recipient);
	
	/* ~ Now, let's use the space we have left to send some data! ~ */
	
	do { *(char *)(destination ++) = *(char *)(current ++); } while ((-- len) && (-- remaining));
	
	if (len) {
		
		/* ~ We need to get another packet! ~ */
		
		target -> bus -> pull(&fmrpacket, sizeof(fmrpacket));
		
		goto in;
		
	}

	
}