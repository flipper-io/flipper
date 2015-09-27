#define __private_include__

#include <fmr/fmr.h>

#include <usart/usart.h>

#include <led/led.h>

#include <platform/atmega.h>

#include <fs/crc.h>

#include <platform/hid.h>

const struct _self self = {
	
	self_configure,
	
	self_invoke,
		
	self_push,
	
	self_pull
	
};

void self_configure(const struct _bus *bus) {
	
	
	
}

uint32_t self_call(void) {
	
	/* ~ Dereference a pointer to the targeted object. ~ */
	
	void *object = (void *)(pgm_read_word(&objects[fmrpacket.recipient.object]));
	
	/* ~ Dereference a pointer to the targeted function. ~ */
	
	void *function = ((void **)(object))[fmrpacket.recipient.index];
	
	/* ~ Invoke the targeted function with the appropriate arguments. ~ */
	
	fmr_call(function, fmrpacket.recipient.argc, &fmrpacket.body);
	
	return *(uint32_t *)(&fmrpacket.body);
	
}

uint32_t self_invoke(const struct _target *sender) {
	
	/* ~ Compare the checksums of the packets to ensure the data was sent successfully. ~ */
	
	uint16_t cs = checksum((void *)(&fmrpacket.recipient.object), fmrpacket.header.length - sizeof(struct _fmr_header));
	
	/* ~ If the checksums are different, then we have a problem. ~ */
	
	if (cs != fmrpacket.header.checksum) {
		
		led_set_rgb(25, 0, 0);
		
		return 0;
	
	}
	
	uint32_t retval = self_call();
	
	/* ~ Return whatever we received back to the device that sent us a message. ~ */
	
	sender -> bus -> push(&retval, sizeof(uint32_t));
	
	return 0;
	
}

char ppbuf[128];

uint32_t self_push(uint8_t object, uint8_t index, uint8_t argc, uint32_t length) {
	
	void *destination = malloc(length);
	
	char *argv = malloc(argc);
	
	if (!destination || !argv) {
		
		led_set_rgb(25, 0, 0);
		
		return 0;
		
	}
	
	memcpy(argv, (void *)(fmrpacket.body) + 10, argc);
	
	/* -------------------------------------- ACTUALLY MOVE THE DATA -------------------------------------- */
	
	uint32_t len = length;
	
	size_t remaining = FLIPPER_DATAGRAM_SIZE - (sizeof(struct _fmr_packet) - FMR_BODY_SIZE + FMR_PUSH_PARAMETER_SIZE + argc);
	
	void *dest = destination;
	
	char *current = (char *)(fmrpacket.body + FMR_PUSH_PARAMETER_SIZE + argc);
	
in:
	
	/* ~ Now, let's use the space we have left to send some data! ~ */
	
	do { *(char *)(dest ++) = *(char *)(current ++); } while ((-- len) && (-- remaining));
	
	if (len) {
		
		/* ~ We need to get another packet! ~ */
		
		usb_receive_packet((void *)(&fmrpacket));
		
		current = (char *)(&fmrpacket.recipient);
		
		remaining = FLIPPER_DATAGRAM_SIZE - sizeof(struct _fmr_header);
		
		goto in;
		
	}
	
	fmrpacket.recipient.object = object;
	
	fmrpacket.recipient.index = index;
	
	fmrpacket.recipient.argc = argc + 6;
	
	fmrpacket.body[0] = hi((uint16_t)(destination));
	
	fmrpacket.body[1] = lo((uint16_t)(destination));
	
	fmrpacket.body[2] = hi(hi16(length));
	
	fmrpacket.body[3] = lo(hi16(length));
	
	fmrpacket.body[4] = hi(lo16(length));
	
	fmrpacket.body[5] = lo(lo16(length));
	
	memmove((void *)(fmrpacket.body) + 7, argv, argc);
	
	free(argv);
	
	uint32_t retval = self_call();
	
	free(destination);
	
	return retval;
	
}

void self_pull(uint8_t object, uint8_t index, uint8_t argc, uint32_t length) {
	
	void *source = ppbuf;
	
	fmrpacket.recipient.object = object;
	
	fmrpacket.recipient.index = index;
	
	fmrpacket.recipient.argc = argc + 6;
	
	fmrpacket.body[0] = hi((uint16_t)(source));
	
	fmrpacket.body[1] = lo((uint16_t)(source));
	
	fmrpacket.body[2] = hi(hi16(length));
	
	fmrpacket.body[3] = lo(hi16(length));
	
	fmrpacket.body[4] = hi(lo16(length));
	
	fmrpacket.body[5] = lo(lo16(length));
	
	memmove((void *)(fmrpacket.body) + 6, (void *)(fmrpacket.body) + 10, argc);
	
	self_call();
	
	
	size_t remaining;
	
	char *current;
	
	/* ~ Construct the header of the packet. ~ */
	
	fmrpacket.header.fe = 0xFE;
	
	fmrpacket.header.length = sizeof(struct _fmr_header);
	
out:
	
	/* ~ Okay, we've now consumed quite a bit of the maximum FLIPPER_DATAGRAM_SIZE. How much do we have left? ~ */
	
	remaining = FLIPPER_DATAGRAM_SIZE - sizeof(struct _fmr_header);
	
	current = (char *)(&fmrpacket.recipient);
	
	/* ~ Now, let's use the space we have left to send some data! ~ */
	
	do { *(char *)(current ++) = *(char *)(source ++); (fmrpacket.header.length) ++; } while ((-- length) && (-- remaining));
	
	fmrpacket.header.checksum = checksum((void *)(&(fmrpacket.recipient)), (fmrpacket.header.length) - sizeof(struct _fmr_header));
	
	usb_send_packet((char *)(&fmrpacket));
	
	if (length) {
		
		/* ~ We need to send another packet! ~ */
		
		fmrpacket.header.length = sizeof(struct _fmr_header);
		
		goto out;
		
	}
	
}