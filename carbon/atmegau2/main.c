#include <flipper.h>

#include <flipper/atmegau2/megausb.h>

static struct _fmr_packet packet;
static struct _lf_device *_u2;

int debug_putchar(char c, FILE *stream) {
	usb_debug_putchar(c);
	return 0;
}

void loop(void) {

	int e;

	while (1) {

		/* pet the watchdog */
		wdt_reset();

		/* obtain a message runtime packet */
		e = megausb_bulk_receive(&packet, sizeof(struct _fmr_packet));

		/* ensure the packet is valid */
		lf_assert(e, E_USB, "failed to obtain usb packet");

		lf_debug("performing");

		/* execute the packet */
		fmr_perform(_u2, &packet);

fail:
		led_rgb(10, 0, 0);
		continue;
	}
}

/* Use USB as the read/write endpoint. */

int atmegau2_read(struct _lf_device *device, void *dst, uint32_t length) {
	return megausb_bulk_receive(dst, length);
}

int atmegau2_write(struct _lf_device *device, void *src, uint32_t length) {
	return megausb_bulk_transmit(src, length);
}

int atmegau2_release(struct _lf_device *device) {
	return lf_error;
}

int main(void) {

	/* Configure the AVR. */
	//wdt_enable(WDTO_500MS);
	CLKPR = (1 << CLKPCE);
	CLKPR = 0;
	sei();

	/* Configure peripheral pins. */
	SAM_POWER_PORT = (1 << SAM_POWER_PIN);
	SAM_POWER_DDR = (1 << SAM_POWER_PIN) | (1 << SAM_RESET_PIN) | (1 << SAM_TEST_PIN) | (1 << SAM_ERASE_PIN);
	FLASH_CS_PORT |= (1 << FLASH_CS_PIN);
	FLASH_CS_DDR &= ~(1 << FLASH_CS_PIN);
	FLASH_WP_PORT |= FLASH_WP_PIN;
	FLASH_WP_DDR |= FLASH_WP_PIN;
	FLASH_RESET_PORT |= FLASH_RESET_PIN;
	FLASH_RESET_DDR |= FLASH_RESET_PIN;

	/* Configure the button to reset. */
	PCMSK1 |= (1 << PCINT8);
	PCICR |= (1 << PCIE1);

	/* Create a flipper device. */
#warning Make this static.

	_u2 = lf_device_create(atmegau2_read, atmegau2_write, atmegau2_release);
	lf_attach(_u2);

	extern struct _lf_module button;
	extern struct _lf_module gpio;
	extern struct _lf_module led;
	extern struct _lf_module spi;
	extern struct _lf_module uart0;
	extern struct _lf_module wdt;

	/* Register all of the modules on this device. */
	dyld_register(_u2, &button);
	dyld_register(_u2, &gpio);
	dyld_register(_u2, &led);
	dyld_register(_u2, &spi);
	dyld_register(_u2, &uart0);
	dyld_register(_u2, &wdt);

	/* Configure all of the peripheral drivers. */
	button_configure();
	gpio_configure();
	led_configure();
	spi_configure();
	uart0_configure();
	wdt_configure();

	/* connect to the USB host */
	usb_configure();

	/* Use USB debug as STDOUT. */
	FILE debug_f = FDEV_SETUP_STREAM(debug_putchar, NULL, _FDEV_SETUP_RW);
	stdout = &debug_f;

	/* Bring the 4S out of reset. */
	SAM_POWER_PORT |= (1 << SAM_RESET_PIN);

	led_rgb(LED_GREEN);

	/* Run the main loop. */
	loop();
}

/* PCINT8 interrupt service routine; captures reset button press and resets the device using the WDT. */
ISR (PCINT1_vect) {
	led_rgb(LED_OFF);
	SAM_RESET_PORT &= ~(1 << SAM_RESET_PIN);
	SAM_POWER_PORT &= ~(1 << SAM_POWER_PIN);
	wdt_enable(WDTO_15MS);
	while (1) __asm__ __volatile__("nop");
}
