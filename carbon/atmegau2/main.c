#include <flipper.h>

#include <flipper/atmegau2/megausb.h>

int debug_putchar(char c, FILE *stream) {
	usb_debug_putchar(c);
	return 0;
}

void loop(void) {
	while (1) {

		struct _fmr_packet packet;
		struct _fmr_result result;

		int _e = megausb_bulk_receive(&packet, sizeof(struct _fmr_packet));

		wdt_reset();

		if (_e == lf_success) {
			lf_error_clear();
			fmr_perform(&packet, &result);
			megausb_bulk_transmit(&result, sizeof(struct _fmr_result));
		}

		wdt_reset();

		__asm__ __volatile__("nop");
	}
}

int main(void) {

	wdt_enable(WDTO_500MS);

	CLKPR = (1 << CLKPCE);
	CLKPR = 0;

	sei();

	/* Power on the 4S, put it in reset. */
	SAM_POWER_PORT = (1 << SAM_POWER_PIN);
	SAM_POWER_DDR = (1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN) | (1 << SAM_TEST_PIN) | (1 << SAM_ERASE_PIN);

	FLASH_CS_PORT |= (1 << FLASH_CS_PIN);
	FLASH_CS_DDR &= ~(1 << FLASH_CS_PIN);
	FLASH_WP_PORT |= FLASH_WP_PIN;
	FLASH_WP_DDR |= FLASH_WP_PIN;
	FLASH_RESET_PORT |= FLASH_RESET_PIN;
	FLASH_RESET_DDR |= FLASH_RESET_PIN;

	PCMSK1 |= (1 << PCINT8);
	PCICR |= (1 << PCIE1);

	struct _lf_device *_u2 = lf_device_create("atmegau2", NULL);
	lf_attach(_u2);

	extern struct _lf_module button;
	extern struct _lf_module gpio;
	extern struct _lf_module led;
	extern struct _lf_module spi;
	extern struct _lf_module uart0;
	extern struct _lf_module wdt;

	dyld_register(_u2, &button);
	dyld_register(_u2, &gpio);
	dyld_register(_u2, &led);
	dyld_register(_u2, &spi);
	dyld_register(_u2, &uart0);
	dyld_register(_u2, &wdt);

	/* Use USB debug as STDOUT. */
	FILE debug_f = FDEV_SETUP_STREAM(debug_putchar, NULL, _FDEV_SETUP_RW);
	stdout = &debug_f;

	led_rgb(LED_GREEN);

	TCCR1B |= (1 << WGM12);
	OCR1A = 15625; // 1s
	TIMSK1 |= (1 << OCIE1A);
	TCCR1B |= (1 << CS12) | (0 << CS11) | (1 << CS10);

	/* Bring the 4S out of reset. */
	SAM_POWER_PORT |= (1 << SAM_RESET_PIN);

	/* Run the main loop. */
	loop();

	/* Loop here if the kernel were ever to reach an unknown execution state. */
	while (1) __asm__ __volatile__("nop");
}

/* PCINT8 interrupt service routine; captures reset button press and resets the device using the WDT. */
ISR (PCINT1_vect) {
	led_rgb(LED_OFF);
	SAM_RESET_PORT &= ~(1 << SAM_RESET_PIN);
	SAM_POWER_PORT &= ~(1 << SAM_POWER_PIN);
	wdt_enable(WDTO_15MS);
	while (1) __asm__ __volatile__("nop");
}

ISR(TIMER1_COMPA_vect) {
	// static int b = 0;
	// led_rgb(0, 0, (b^=1)*10);
	// static int heartbeat = 0;
	// uint8_t _sreg = SREG;
	// cli();
	// printf("%i\n", heartbeat++);
	// SREG = _sreg;
}
