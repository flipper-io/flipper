#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <platform/atmega.h>

#include <platform/hid.h>

fmr_packet fmrpacket;

const void * const objects[] PROGMEM = { &button, &flash, &host, &self, &device, &fs, &i2c, &io, &led, &pwm, &spi, &timer, &usart, &usart, &usart, &usb, &wifi };

struct _target *sender;

extern void fmr_broadcast(void) {
	
	sender -> bus -> push(&fmrpacket, (fmrpacket.header.length));
	
}

/* ~ Populate the global fmr_packet for use by the message runtime itself. Handles implementations of different busses. ~ */

void fmr_retrieve(void) {
	
	if (sender == &device) {
		
		usb_receive_packet((void *)(&fmrpacket));
		
	}
	
	else if (sender == &host) {
		
		disable_interrupts();
		
		while (usart0_get() != 0xFE);
		
		struct _fmr_header *header = &(fmrpacket.header);
		
		/* ~ Load the header of the packet. ~ */
		
		for (unsigned i = 1; i < sizeof(struct _fmr_header); i ++) ((char *)(header))[i] = usart0_get();
		
		/* ~ Load the body of the packet. ~ */
		
		for (unsigned i = 0; i < (header -> length); i ++) ((char *)(&fmrpacket.recipient.object))[i] = usart0_get();
		
		while (usart0_ready()) { (void)usart0_get(); }
		
		enable_interrupts();
		
	}
	
}

/* ~ USB receive interrupt. ~ */


void usb_receive_interrupt(void) {
	
	sender = &device;
	
	self_invoke(&device);
	
}

/* ~ USART recieve interrupt. ~ */

ISR(USART1_RX_vect) {
	
	sender = &host;
	
	fmr_retrieve();
	
	self_invoke(&host);
	
}