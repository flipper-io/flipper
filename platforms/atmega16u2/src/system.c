#define __private_include__
#include <platform/atmega16u2.h>
#include <private/usb.h>
#include <flipper/uart.h>
#include <flipper/led.h>

#define cpu_prescale(clock) (CLKPR = 0x80, CLKPR = clock)

void system_task(void) {
	while (1) {
		char *packet[32];
		int8_t _e = interrupt_receive_packet((void *)(packet));
		if (_e > 0) {

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
