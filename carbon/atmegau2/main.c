#define __private_include__
#include <flipper.h>
#include <flipper/atmegau2/atmegau2.h>
#include <flipper/atmegau2/megausb.h>

void loop(void) {
	while (1) {

		struct _fmr_packet packet;
		struct _fmr_result result;

		int _e = megausb_bulk_receive(&packet, sizeof(struct _fmr_packet));
		if (_e == lf_success) {
			lf_error_clear();
			fmr_perform(&packet, &result);
			megausb_bulk_transmit(&result, sizeof(struct _fmr_result));
		}

		__asm__ __volatile__("nop");
	}
}

int main(void) {

	wdt_disable();

	CLKPR = (1 << CLKPCE);
	CLKPR = 0;

	sei();

	SAM_POWER_PORT &= ~(1 << SAM_POWER_PIN); // 4S is off
	SAM_POWER_DDR |= (1 << SAM_POWER_PIN);
	SAM_TEST_PORT |= (1 << SAM_TEST_PIN); // 4S is not in test mode
	SAM_TEST_DDR |= (1 << SAM_TEST_PIN);
	SAM_RESET_PORT &= ~(1 << SAM_RESET_PIN); // 4S is in reset state
	SAM_RESET_DDR |= (1 << SAM_RESET_PIN);
	SAM_ERASE_PORT &= ~SAM_ERASE_PIN; // 4S is not being erased
	SAM_ERASE_DDR |= (1 << SAM_ERASE_PIN);

	PCMSK1 |= (1 << PCINT8);
	PCICR |= (1 << PCIE1);

	button_configure();
	led_configure();
	usb_configure();

	led_rgb(LED_GREEN);

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
