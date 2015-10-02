#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <usart/usart.h>

#include <platform/hid.h>

const void * const objects[] PROGMEM = { &button, &flash, &host, &self, &device, &fs, &i2c, &io, &led, &pwm, &spi, &timer, &usart, &usart, &usart, &usb, &wifi };

void usb_receive_interrupt(void) {
	
	/* ~ Associate this interrupt with the device target. ~ */
	
	fmr_associate_target(&device);
	
	/* ~ Invoke the FMR. ~ */
	
	self_invoke(&device);
	
}

/* ~ USART recieve interrupt. ~ */

ISR(USART1_RX_vect) {
	
	/* ~ Associate this interrupt with the host target. ~ */
	
	fmr_associate_target(&host);
	
	/* ~ Disable interrupts to prevent alignment issues. ~ */
	
	disable_interrupts();
	
	/* ~ Load a packet from the bus. ~ */
	
	fmr_retrieve(0);
	
	/* ~ Invoke the FMR. ~ */
	
	self_invoke(&host);

	
}