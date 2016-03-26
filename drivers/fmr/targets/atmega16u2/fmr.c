#define __private_include__
#include <fmr/fmr.h>
#include <platform.h>
#include <flipper/drivers.h>

const void * const objects[] PROGMEM = { &host, &device, &self, &led, &button, &error, &usart, &spi, &sam, &at45, &fs, &usb, &wifi };

void usb_receive_interrupt(void) {

	/* ~ Associate this interrupt with the device target. ~ */
	fmr_associate_target(&host);

	/* ~ Invoke the FMR. ~ */
	fmr_parse(&host);
}

/* ~ USART recieve interrupt. ~ */
ISR(USART1_RX_vect) {

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

}
