#define __private_include__
#include <fmr/fmr.h>
#include <platform.h>
#include <flipper/flipper.h>

const void * const objects[] PROGMEM = { &host, &device, &self };

void fmr_configure(void) {

}

fmr_handle fmr_bind(uint16_t bundle) {

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
