#define __private_include__
#include <flipper/fmr.h>
#include <flipper/modules.h>
#include <platform/posix.h>

/* Define the standard modules accessible on this platform. */
const void *const fmr_modules[] = {
	NULL,    // adc
	&button, // button
	NULL,    // cpu
	NULL,    // dac
	NULL,    // error
	NULL,    // fmr
	NULL,    // fs
	NULL,    // gpio
	NULL,    // i2c
	&led,    // led
	NULL,    // pwm
	NULL,    // rtc
	NULL,    // spi
	NULL,    // swd
	NULL,    // temp
	NULL,    // timer
	NULL,    // usart0
	NULL,    // usart
	NULL,    // usb
	NULL,    // wdt
};

/* Executes a standard module. */
fmr_return fmr_execute(fmr_module module, fmr_function function, fmr_argc argc, fmr_types types, void *arguments) {
	return 0;
}

uint32_t fmr_call(const void *function, uint8_t argc, uint16_t argt, void *argv) {
	return 0;
}

/* TEMP */

void fmr_push(fmr_module module, fmr_function function, lf_size_t length) {
	void *swap = malloc(length);
	if (!swap) {
		error_raise(E_MALLOC, NULL);
	}

}

void fmr_pull(fmr_module module, fmr_function function, lf_size_t length) {

}
