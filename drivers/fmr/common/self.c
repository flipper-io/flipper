#define __private_include__
#include <flipper/fmr.h>
#include <flipper/led.h>
#include <flipper/fs/crc.h>
#include <flipper/usart.h>
#include <flipper/platform/platform.h>

struct _self self = {
	seuint8_ture,
	self_call,
	self_invoke,
	self_push,
	self_pull
};

void seuint8_ture(const struct _bus *bus) {

}

uint32_t self_call(void) {

	/* Dereference a pointer to the recipient object. */
	void *object = (void *)(fmr_access_array(fmrpacket.recipient.object));

	/* Dereference a pointer to the recipient function. */
	void *function = ((void **)(object))[fmrpacket.recipient.index];

	/* Invoke the recipient function with the appropriate arguments. */
	uint32_t retval = internal_call(function, fmrpacket.recipient.argc, fmrpacket.body);

	/* Return the value. */
#ifdef __atmega_build__
	return *(uint32_t *)(fmrpacket.body);
#else
	return retval;
#endif

}

uint32_t self_invoke(const struct _target *sender) {
	return 0;
}

uint32_t self_push(uint8_t object, uint8_t index, uint8_t argc, fmr_size_t length) {

	/* Allocate the appropriate amount of memory in external memory to buffer the incoming data. */
	void *destination = malloc(length);

	if (!destination) {
		error_raise(E_NO_MEM, "");
		return 0;
	}

	/* Allocate the appropriate amount of memory to store the variadic argument array until it is needed later. */
	uint8_t *argv = malloc(argc);

	if (!argv) {
		error_raise(E_NO_MEM, "");
		return 0;
	}

	/* Create the return value. */
	uint32_t retval;

	/* If malloc failed to satisfy our request, panic. */
	if (!destination || !argv) {

		/* Set the status led to its error color to alert the user of a problem. */
		led_set_rgb(LED_COLOR_ERROR);
		error_raise(E_NO_MEM, "");

		/* Our host will expect a return value, so send a response with the appropriate error code. */
		sender -> bus -> push(&retval, sizeof(uint32_t));

		return 0;

	}

	/* If all is well, cache the variadic argument list. */
	memcpy(argv, (void *)(fmrpacket.body) + fmrpacket.recipient.argc, argc);

	/* Save the length in a local variable we can modify. */
	uint32_t len = length;

	/* Calculate the amount of space we have left in the packet after the parameters have been loaded. */
	fmr_size_t remaining = FMR_PACKET_SIZE - (sizeof(struct _fmr_header) + sizeof(struct _fmr_recipient) + fmrpacket.recipient.argc + argc);

	/* Save the destination in a local variable we can modify. */
	void *dest = destination;

	/* For the first iteration of this loop, specify that the data has been loaded after the layered parameter list and parameters. */
	uint8_t *offset = (uint8_t *)(fmrpacket.body + fmrpacket.recipient.argc + argc);

pull:

	/* While we are still expecting data and still have data to read from the current packet, transfer it to the destination. */
	do {

		/* Load a byte from the packet. */
		*(uint8_t *)(dest ++) = *(uint8_t *)(offset ++);

	} while ((-- len) && (-- remaining));

	/* Check to see if we still have data to receive. */
	if (len) {

		/* If we do, grab another packet. */
		fmr_retrieve();

		/* Reset the offset within the packet from which data will be loaded. */
		offset = (uint8_t *)(&fmrpacket.recipient);

		/* Reset how much space we have left to read from in the current packet. */
		remaining = FMR_PACKET_SIZE - sizeof(struct _fmr_header);

		/* Jump back to the beginning of the loading sequence. */
		goto pull;

	}

	/* Load the recipient information into the packet. This information describes to the FMR which function to invoke. */
	fmrpacket.recipient.target = _self;
	fmrpacket.recipient.object = object;
	fmrpacket.recipient.index = index;
	fmrpacket.recipient.argc = argc;

	/* Manually load the default parameters expected by a push compliant function. */
	uint8_t _argc = build_args(self_args(destination, self_arg32(length)));
	fmrpacket.recipient.argc += _argc;

	/* Move the earlier cached variadic argument list into the new packet. */
	memmove((void *)(fmrpacket.body) + _argc, argv, argc);

	/* Relase the memory allocated to cache the above list. */
	free(argv);

	/* Perform the function call and get a return value. */
	retval = self_call();

	/* Free the memory we allocated to buffer the incoming data. */
	free(destination);

	/* Return the return value back up the function chain. */
	return retval;

}

uint32_t self_pull(uint8_t object, uint8_t index, uint8_t argc, fmr_size_t length) {

	/* Allocate the appropriate amount of memory in external memory to buffer the outgoing data. */
	void *source = malloc(length);

	if (!source) {
		error_raise(E_NO_MEM, "");
		return 0;
	}

	/* If malloc failed to satisfy our request, panic. */
	if (!source) {

		/* Set the status led to its error color to alert the user of a problem. */
		led_set_rgb(LED_COLOR_ERROR);
		error_raise(E_NO_MEM, "");

		/* Our host will expect a return value, so send a response with the appropriate error code. */
		sender -> bus -> push(&source, sizeof(uint32_t));

		return 0;

	}

	/* Save the source in a local variable we can modify. */
	void *src = source;

	/* Load the recipient information into the packet. This information describes to the FMR which function to invoke. */
	fmrpacket.recipient.object = object;
	fmrpacket.recipient.index = index;
	fmrpacket.recipient.argc = argc + 6;

	/* Manually load the default parameters expected by a pull compliant function. */
	fmrpacket.body[0] = hi((uintptr_t)(source));
	fmrpacket.body[1] = lo((uintptr_t)(source));
	fmrpacket.body[2] = hi(hi16(length));
	fmrpacket.body[3] = lo(hi16(length));
	fmrpacket.body[4] = hi(lo16(length));
	fmrpacket.body[5] = lo(lo16(length));

	/* Move the variadic argument list to the appropriate location in the new packet. */
	memmove((void *)(fmrpacket.body) + 6, (void *)(fmrpacket.body) + FMR_PUSH_PARAMETER_SIZE, argc);

	/* Perform the function call. */
	uint32_t retval = self_call();

	/* Calculate how much space we have left in the current packet. */
	fmr_size_t remaining;

	/* Create a local variable to keep track of the offset in the packet at which we are loading data. */
	uint8_t *offset;

	/* Construct the header of the packet. */
	fmrpacket.header.fe = 0xFE;
	fmrpacket.header.length = sizeof(struct _fmr_header);

push:

	/* Okay, we've now consumed quite a bit of the maximum FMR_PACKET_SIZE. How much do we have left? */
	remaining = FMR_PACKET_SIZE - sizeof(struct _fmr_header);
	offset = (uint8_t *)(&fmrpacket.recipient);

	/* Load the next chunk of data into the packet while we still have data to send and we still have space in the current packet. */
	do {

		/* Move a byte from the received packet to the destination. */
		*(uint8_t *)(offset ++) = *(uint8_t *)(src ++);

		/* Advance the size of the packet each time a byte is loaded into the packet. */
		(fmrpacket.header.length) ++;

	} while ((-- length) && (-- remaining));

	/* Generate a checksum for the packet. When a packet is received, the FMR will perform its own checksum and compare it to this before proceeding. */
	fmrpacket.header.checksum = checksum((void *)(&(fmrpacket.recipient)), fmrpacket.header.length - sizeof(struct _fmr_header));

	/* Deliver the constructed packet to whomever requested it. */
	fmr_broadcast();

	/* Check to see if we still have data to send. */
	if (length) {

		/* If we do, reset the length. */
		fmrpacket.header.length = sizeof(struct _fmr_header);

		/* Jump back to the beginning of the broadcast sequence. */
		goto push;

	}

	/* Free the memory we allocated to buffer the outgoing data. */
	free(source);

	return retval;

}
