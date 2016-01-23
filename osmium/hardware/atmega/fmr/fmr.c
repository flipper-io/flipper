#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

#include <platform/atmega.h>

const void * const objects[] PROGMEM = { &host, &device, &self, &button, &at45, &fs, &i2c, &io, &led, &pwm, &sam, &spi, &timer, &usart, &usart, &usart, &usb, &wifi, &fdl, &fmr };

void fmr_configure(void) {
	
	
	
}

void fmr_bind(fmr_handle *handle, uint16_t id) {
	
	
	
}

uint32_t fmr_invoke(fmr_handle handle, uint8_t index, uint8_t argc, ...) {
	
	return 0;
	
}

void *fmr_resolve(void *source, uint32_t length) {
	
	return NULL;
	
}

void usb_receive_interrupt(void) {
	
	/* ~ Associate this interrupt with the device target. ~ */
	
	fmr_associate_target(&host);
	
	/*  ~ Alert the system that the FMR is busy. ~ */
	
	fmr_busy = true;
	
	/* ~ Invoke the FMR. ~ */
	
	fmr_parse(&host);
	
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
	
	fmr_parse(&device);
	
	/* ~ Re-enable interrupts. ~ */
	
	enable_interrupts();
	
	/* ~ Free the FMR. ~ */
	
	fmr_busy = false;

	
}