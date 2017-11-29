#define __private_include__
#include <flipper.h>
#include <flipper/atmegau2/atmegau2.h>
#include <flipper/atmegau2/megausb.h>

extern void usb_debug_putchar(char c);

int debug_putchar(char c, FILE *stream) {
	usb_debug_putchar(c);
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

	SAM_POWER_PORT = (1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN);
	SAM_POWER_DDR = (1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN) | (1 << SAM_TEST_PIN) | (1 << SAM_ERASE_PIN);

	PCMSK1 |= (1 << PCINT8);
	PCICR |= (1 << PCIE1);

	button_configure();
	led_configure();
	usb_configure();
	uart0_configure(NULL);

	/* Use USB debug as STDOUT. */
	FILE debug_f = FDEV_SETUP_STREAM(debug_putchar, NULL, _FDEV_SETUP_RW);
	stdout = &debug_f;

	led_rgb(LED_GREEN);

	TCCR1B |= (1 << WGM12);
	OCR1A = 15625; // 1s
	TIMSK1 |= (1 << OCIE1A);
	TCCR1B |= (1 << CS12) | (0 << CS11) | (1 << CS10);

	/* Run the main loop. */
	loop();

	/* Loop here if the kernel were ever to reach an unknown execution state. */
	while (1) __asm__ __volatile__("nop");
}

/* PCINT8 interrupt service routine; captures reset button press and resets the device using the WDT. */
ISR (PCINT1_vect) {
	led_rgb(LED_OFF);
	wdt_enable(WDTO_15MS);
	while (1) __asm__ __volatile__("nop");
}

ISR(TIMER1_COMPA_vect) {
	static int heartbeat = 0;
	printf("%i\n", heartbeat++);
}
