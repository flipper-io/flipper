#define __private_include__
#include <platform/atmega16u2.h>
#include <private/megausb.h>
#include <flipper/uart.h>
#include <flipper/led.h>

/* The fmr_device object containing global state about this device. */
struct _lf_device self = {
	NULL,
	0,
	0,
	&megausb,
	E_OK,
	false,
	NULL
};

#define cpu_prescale(clock) (CLKPR = 0x80, CLKPR = clock)

uint32_t retcat(uint16_t l, uint16_t h) {
	error_raise(E_FMR, NULL);
	return ((uint32_t)h << 16) | l;
}

void system_task(void) {
	while (1) {
		struct _fmr_packet packet;
		int8_t _e = megausb_pull((void *)(&packet), sizeof(struct _fmr_packet));
		if (_e > 0) {
			/* Create a buffer to the result of the operation. */
			struct _fmr_result result;
			/* Execute the packet. */
			fmr_perform(&packet, &result);
			/* Send the result back to the host. */
			megausb_push(&packet, sizeof(struct _fmr_result));
		}
	}
}

void system_init() {
	/* Prescale CPU for maximum clock. */
	cpu_prescale(0);
	/* Configure the USB subsystem. */
	configure_usb();
	/* Configure the UART subsystem. */
	uart_configure();
	/* Configure reset button and PCINT8 interrupt. */
	DDRC &= ~(1 << 6);
	PCMSK1 |= (1 << PCINT8);
	PCICR |= (1 << PCIE1);
	sei();
}

void system_deinit(void) {
	/* Clear the state of the status LED. */
	led_set_rgb(LED_OFF);
}

/* PCINT8 interrupt service routine; captures reset button press and resets device. */
ISR (PCINT1_vect) {
	/* Shutdown the system. */
	system_deinit();
	/* Enable the watchdog. */
	wdt_enable(WDTO_15MS);
	/* Wait until reset. */
	while (1);
}
