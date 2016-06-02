#define __private_include__
#include <flipper/button.h>
#include <flipper/platform/platform.h>
#include <flipper/led.h>

void button_configure(void) {
	/* Enable the button as an input. */
	clear_bit_in_port(BUTTON_PIN, BUTTON_DDR);
	/* Enable the reset button interrupt. */
	PCMSK1 |=  (1 << PCINT8);
	PCICR  |=  (1 << PCIE1);
}

ISR (PCINT1_vect) {
	/* Disable all interrupts. */
	disable_interrupts();
	/* Turn off the LED. */
	led_set_rgb(0, 0, 0);
	/* The watchdog timer is used to generate a hardware reset on the U2. Configure it to time out in 15 ms. */
	wdt_enable(WDTO_15MS);
	/* Wait for imminent doom. */
	while (true);
}

uint8_t button_read(void) {
	return (BUTTON_IN & 1);
}
