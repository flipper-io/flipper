#define __private_include__

#include <fmr/bus.h>

#include <fs/crc.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

/* ~ This function ensures that the target still has a heartbeat. ~ */

void validate_target(const struct _target *target) {
	
	/* ~ If we've been asked to send a packet but have no target, this is a dangling instance of libflipper. ~ */
	
	if (!(target -> bus)) {
		
		/* ~ Report the issue to the user and panic. ~ */
		
		error("Oops. No device has been attached to this instance of libflipper.\n\n"
			  
			  "Make sure you specify how your Flipper device is connected before talking to it: USB, Wi-Fi, and Bluetooth are currently supported.\n\n"
			  
			  "See http://api.flipper.io/attatch for more information.\n\n");
		
		exit(EXIT_FAILURE);
		
	}
	
}

/* ~ This function wraps up a message needed to perform a remote procedure call and sends it off to its target. ~ */

uint32_t target_invoke(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, va_list *argv) {
	
	/* ~ Associate this request with the appropriate target. ~ */
	
	fmr_associate_target(target);
	
	/* ~ Ensure we are talking to a valid target. ~ */
	
	validate_target(target);
	
	/* ~ Since our target has 32-bit registers, we package each argument in a 32-bit container. ~ */
	
	argc = argc * sizeof(uint16_t);
	
	/* ~ Set the first byte of the packet to 0xFE, an arbitrary unique identifier that gives the FMR the ability to detect the start of a packet. ~ */
	
	fmrpacket.header.fe = 0xFE;
	
	/* ~ Set the header's length equal to the size of the full packet after the parameters have been loaded. ~ */
	
	fmrpacket.header.length = sizeof(struct _fmr_header) + sizeof(struct _fmr_recipient) + argc;
	
	/* ~ Ensure the arguments will fit into one packet. ~ */
	
	if (fmrpacket.header.length > FLIPPER_DATAGRAM_SIZE) {
		
		verbose("\tError. Too many arguments provided for the requested function call. Skipping.\n\n");
		
		return 0;
		
	}
	
	/* ~ Load the recipient information into the packet. This information describes to the FMR which function to invoke. ~ */
	
	fmrpacket.recipient.target = target -> id;
	
	fmrpacket.recipient.object = object;
	
	fmrpacket.recipient.index = index;
	
	fmrpacket.recipient.argc = argc;
	
	/* ~ The packet's body will contain the parameters which will be passed to the function. ~ */
	
	for (unsigned i = 0; i < argc; i += 2) {
		
		/* ~ Unstage an argument from the variadic argument list. ~ */
		
		unsigned arg = va_arg(*argv, unsigned);
		
		/* ~ Load it into the packet. ~ */
		
		fmrpacket.body[i] = hi(arg); fmrpacket.body[i + 1] = lo(arg);
		
	}
	
	/* ~ Release the variadic argument list. ~ */
	
	va_end(*argv);
	
	/* ~ Generate a checksum for the packet. When a packet is received, the FMR will perform its own checksum and compare it to this before proceeding. ~ */
	
	fmrpacket.header.checksum = checksum((void *)(&fmrpacket.recipient), fmrpacket.header.length - sizeof(struct _fmr_header));
	
	/* ~ Send the constructed packet to the target. ~ */
	
	fmr_broadcast();
	
	/* ~ We will now expect the FMR to send us a packet in return, acknowledging that the packet has been received successfully. ~ */
	
	/* ~-- TO BE IMPLEMENTED --~ */
	
	/* ~ Use a local variable to store the return value. ~ */
	
	uint32_t retval;
	
	/* ~ Load the value that the function returned from the target. ~ */
	
	target -> bus -> pull(&retval, sizeof(uint32_t));
	
	/* ~ Return this value back up the function chain. ~ */
	
	return retval;
	
}

/* ~ This function moves data from the isolated address space of the host to the device using the FMR. ~ */

uint32_t target_push(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, void *source, uint32_t length, va_list *argv) {
	
	if (!length) return 0;
	
	/* ~ Associate this request with the appropriate target. ~ */
	
	fmr_associate_target(target);
	
	/* ~ Ensure we are talking to a valid target. ~ */
	
	validate_target(target);
	
	/* ~ Since our target has 32-bit registers, we package each argument in a 32-bit container. ~ */
	
	argc = argc * sizeof(uint16_t);
	
	/* ~ Set the first byte of the packet to 0xFE, an arbitrary unique identifier that gives the FMR the ability to detect the start of a packet. ~ */
	
	fmrpacket.header.fe = 0xFE;
	
	/* ~ Set the header's length equal to the size of the full packet after the parameters have been loaded. ~ */
	
	fmrpacket.header.length = sizeof(struct _fmr_header) + sizeof(struct _fmr_recipient) + FMR_PUSH_PARAMETER_SIZE + argc;
	
	/* ~ Ensure the arguments will fit into one packet. ~ */
	
	if (fmrpacket.header.length > FLIPPER_DATAGRAM_SIZE) {
		
		verbose("\tError. Too many arguments provided for the requested function call. Skipping.\n\n");
		
		return 0;
		
	}
	
	/* ~ Load the recipient information into the packet. This information describes to the FMR which function to invoke. ~ */
	
	fmrpacket.recipient.target = target -> id;
	
	fmrpacket.recipient.object = target -> id;
	
	fmrpacket.recipient.index = _self_push;
	
	fmrpacket.recipient.argc = FMR_PUSH_PARAMETER_SIZE;
	
	/* ~ The packet's body will contain layered information regarding the recipient function that will be processed by the helper function. ~ */
	
	fmrpacket.body[0] = hi(object);
	
	fmrpacket.body[1] = lo(object);
	
	fmrpacket.body[2] = hi(index);
	
	fmrpacket.body[3] = lo(index);
	
	fmrpacket.body[4] = hi(argc);
	
	fmrpacket.body[5] = lo(argc);
	
	fmrpacket.body[6] = hi(hi16(length));
	
	fmrpacket.body[7] = lo(hi16(length));
	
	fmrpacket.body[8] = hi(lo16(length));
	
	fmrpacket.body[9] = lo(lo16(length));
	
	/* ~ If they fit, the parameters which will be passed to the function are loaded into the packet. ~ */
	
	for (int i = 0; i < argc; i += 2) {
		
		unsigned arg = va_arg(*argv, unsigned);
		
		fmrpacket.body[i + FMR_PUSH_PARAMETER_SIZE] = hi(arg); fmrpacket.body[i + FMR_PUSH_PARAMETER_SIZE + 1] = lo(arg);
		
	}
	
	/* ~ For the first iteration of this loop, specify that the data has been loaded after the layered parameter list and parameters. ~ */
	
	uint8_t *offset = (uint8_t *)(fmrpacket.body + FMR_PUSH_PARAMETER_SIZE + argc);
	
	/* ~ Create a local variable to keep track of how much data still needs to be sent. ~ */
	
	uint32_t remaining;
	
push:
	
	/* ~ Calculate how much space we have left in the current packet. ~ */
	
	remaining = FLIPPER_DATAGRAM_SIZE - fmrpacket.header.length;
	
	/* ~ Load the next chunk of data into the packet while we still have data to send and we still have space in the current packet. ~ */
	
	do {
		
		/* ~ Load a byte into the packet. ~ */
		
		*(uint8_t *)(offset ++) = *(uint8_t *)(source ++);
		
		/* ~ Advance the size of the packet each time a byte is loaded into the packet. ~ */
		
		fmrpacket.header.length ++;
	
	} while ((-- length) && (-- remaining));
	
	/* ~ Generate a checksum for the packet. When a packet is received, the FMR will perform its own checksum and compare it to this before proceeding. ~ */
	
	fmrpacket.header.checksum = checksum((void *)(&fmrpacket.recipient), fmrpacket.header.length - sizeof(struct _fmr_header));
	
	/* ~ Send the constructed packet to the target. ~ */
	
	fmr_broadcast();
	
	/* ~ Check to see if we still have data to send. ~ */
	
	if (length) {
		
		/* ~ If we do, reset the length. ~ */
		
		fmrpacket.header.length = sizeof(struct _fmr_header);
		
		/* ~ Reset the offset within the packet from which data will be loaded. ~ */
		
		offset = (uint8_t *)(&fmrpacket.recipient);
		
		/* ~ Jump back to the beginning of the broadcast sequence. ~ */
		
		goto push;
		
	}
	
	/* ~ Use a local variable to store the return value. ~ */
	
	uint32_t retval;
	
	/* ~ Load the value that the function returned from the target. ~ */
	
	target -> bus -> pull(&retval, sizeof(uint32_t));
	
	/* ~ Return this value back up the function chain. ~ */
	
	return retval;
	
}

/* ~ This function moves data from the isolated address space of the device to the host using the FMR. ~ */

void target_pull(const struct _target *target, uint8_t object, uint8_t index, uint8_t argc, void *destination, uint32_t length, va_list *argv) {
	
	if (!length) return;
	
	/* ~ Associate this request with the appropriate target. ~ */
	
	fmr_associate_target(target);
	
	/* ~ Ensure we are talking to a valid target. ~ */
	
	validate_target(target);
	
	/* ~ Since our target has 32-bit registers, we package each argument in a 32-bit container. ~ */
	
	argc = argc * sizeof(uint16_t);
	
	/* ~ Set the first byte of the packet to 0xFE, an arbitrary unique identifier that gives the FMR the ability to detect the start of a packet. ~ */
	
	fmrpacket.header.fe = 0xFE;
	
	/* ~ Set the header's length equal to the size of the full packet after the parameters have been loaded. ~ */
	
	fmrpacket.header.length = sizeof(struct _fmr_packet) - FMR_BODY_SIZE + FMR_PUSH_PARAMETER_SIZE + argc;
	
	/* ~ Ensure the arguments will fit into one packet. ~ */
	
	if (fmrpacket.header.length > FLIPPER_DATAGRAM_SIZE) {
		
		verbose("\tError. Too many arguments provided for the requested function call. Skipping.\n\n");
		
		return;
		
	}
	
	/* ~ Load the recipient information into the packet. This information describes to the FMR which function to invoke. ~ */
	
	fmrpacket.recipient.target = target -> id;
	
	fmrpacket.recipient.object = target -> id;
	
	fmrpacket.recipient.index = _self_pull;
	
	fmrpacket.recipient.argc = FMR_PUSH_PARAMETER_SIZE;
	
	/* ~ The packet's body will contain layered information regarding the recipient function that will be processed by the helper function. ~ */
	
	fmrpacket.body[0] = hi(object);
	
	fmrpacket.body[1] = lo(object);
	
	fmrpacket.body[2] = hi(index);
	
	fmrpacket.body[3] = lo(index);
	
	fmrpacket.body[4] = hi(argc);
	
	fmrpacket.body[5] = lo(argc);
	
	fmrpacket.body[6] = hi(hi16(length));
	
	fmrpacket.body[7] = lo(hi16(length));
	
	fmrpacket.body[8] = hi(lo16(length));
	
	fmrpacket.body[9] = lo(lo16(length));
	
	/* ~ If they fit, the parameters which will be passed to the function are loaded into the packet. ~ */
	
	for (int i = 0; i < argc; i += 2) {
		
		unsigned arg = va_arg(*argv, unsigned);
		
		fmrpacket.body[i + FMR_PUSH_PARAMETER_SIZE] = hi(arg); fmrpacket.body[i + FMR_PUSH_PARAMETER_SIZE + 1] = lo(arg);
		
	}
	
	/* ~ Generate a checksum for the packet. When a packet is received, the FMR will perform its own checksum and compare it to this before proceeding. ~ */
	
	fmrpacket.header.checksum = checksum((void *)(&(fmrpacket.recipient)), (fmrpacket.header.length) - sizeof(struct _fmr_header));
	
	/* ~ Send the constructed packet to the target. ~ */
	
	fmr_broadcast();
	
	/* ~ Retrieve a packet from the target containing the pulled data. ~ */
	
	fmr_retrieve();
	
	/* ~ Create a local variable to keep track of the offset in the packet at which we are loading data. ~ */
	
	uint8_t *offset;
	
	/* ~ Create a local variable to keep track of how much data still needs to be sent. ~ */
	
	size_t remaining;
	
pull:
	
	/* ~ Calculate how much space we have left in the current packet. ~ */
	
	remaining = FLIPPER_DATAGRAM_SIZE - sizeof(struct _fmr_header);
	
	offset = (uint8_t *)(&fmrpacket.recipient);
	
	/* ~ While we are still expecting data and still have data to read from the current packet, transfer it to the destination. ~ */
	
	do {
		
		/* ~ Move a byte from the received packet to the destination. ~ */
		
		*(uint8_t *)(destination ++) = *(uint8_t *)(offset ++);
	
	} while ((-- length) && (-- remaining));
	
	/* ~ Check to see if we still have data to receive. ~ */
	
	if (length) {
		
		/* ~ If we do, grab another packet. ~ */
		
		fmr_retrieve();
		
		/* ~ Jump back to the beginning of the loading sequence. ~ */
		
		goto pull;
		
	}
	
}