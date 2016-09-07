#define __private_include__
#include <platform/atmega16u2.h>
#include <private/usb.h>
#include <flipper/led.h>

#define os_prescale(n) (CLKPR = 0x80, CLKPR = (n))

void system_init() {

	/* Prescale CPU for maximum clock. */
	os_prescale(0);

	/* Configure the USB subsystem. */
	configure_usb();

	/* Configure reset button and PCINT8 interrupt. */
	DDRC &= ~(1 << 6);
	PCMSK1 |= (1 << PCINT8);
	PCICR |= (1 << PCIE1);
	sei();

}

void system_deinit(void) {

	led_set_rgb(0, 0, 0);
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
