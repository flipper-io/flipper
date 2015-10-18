#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <usart/usart.h>

#include <platform/hid.h>

const void * const objects[] PROGMEM = { &button, &flash, &host, &device, &self, &fs, &i2c, &io, &led, &pwm, &sam, &spi, &timer, &usart, &usart, &usart, &usb, &wifi };

void usb_receive_interrupt(void) {
	
	/* ~ Associate this interrupt with the device target. ~ */
	
	fmr_associate_target(&host);
	
	/*  ~ Alert the system that the FMR is busy. ~ */
	
	fmr_busy = true;
	
	/* ~ Invoke the FMR. ~ */
	
	fmr_invoke(&host);
	
	/* ~ Free the FMR. ~ */
	
	fmr_busy = false;
	
}

/* ~ USART recieve interrupt. ~ */

ISR(USART1_RX_vect) {
	
	/* ~ Alert the system that the FMR is busy. ~ */
	
	fmr_busy = true;
	
	/* ~ Associate this interrupt with the host target. ~ */
	
	fmr_associate_target(&device);
	
	/* ~ Disable interrupts to prevent alignment issues. ~ */
	
	disable_interrupts();
	
	/* ~ Load a packet from the bus. ~ */
	
	fmr_retrieve();
	
	/* ~ Invoke the FMR. ~ */
	
	fmr_invoke(&device);
	
	/* ~ Re-enable interrupts. ~ */
	
	enable_interrupts();
	
	/* ~ Free the FMR. ~ */
	
	fmr_busy = false;

	
}