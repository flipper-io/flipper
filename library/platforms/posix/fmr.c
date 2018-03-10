#include <flipper.h>

/* Define the standard modules based on platform specific usage declarations. */
const void *const lf_modules[] = {
	// &adc,
	// &button,
	// &dac,
	// &fld,
	// &gpio,
	// &i2c,
	// &led,
	// &pwm,
	// &rtc,
	// &spi,
	// &swd,
	// &task,
	// &temp,
	// &timer,
	// &uart0,
	// &usart,
	// &usb,
	// &wdt
};

LF_WEAK lf_return_t fmr_call(lf_return_t (* function)(void), lf_type ret, uint8_t argc, uint16_t argt, void *argv) {
	return -1;
}

LF_WEAK lf_return_t fmr_push(struct _fmr_push_pull_packet *packet) {
	return -1;
}

LF_WEAK lf_return_t fmr_pull(struct _fmr_push_pull_packet *packet) {
	return -1;
}
