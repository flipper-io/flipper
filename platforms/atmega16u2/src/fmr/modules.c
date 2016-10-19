#define __private_include__
#include <flipper/fmr.h>
#include <flipper/modules.h>
#include <platform/atmega16u2.h>

/* Define the standard modules accessible on this platform. */
const void *const fmr_modules[] PROGMEM = {
	NULL,    // adc
	&button, // button
	&cpu,    // cpu
	NULL,    // dac
	NULL,    // error
	NULL,	 // fld
	&fmr,    // fmr
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
	&uart0,  // usart0
	NULL,    // usart
	NULL,    // usb
	NULL,    // wdt
};
/* NOTE: PROGMEM is used here to store this array within flash memory of this platform. */

/* Executes a standard module. */
fmr_return fmr_execute(fmr_module module, fmr_function function, fmr_argc argc, fmr_types types, void *arguments) {
	/* Dereference the pointer to the target module. */
	void *object = (void *)(pgm_read_word(&fmr_modules[module]));
	/* Dereference and return a pointer to the target function. */
	void *address = ((void **)(object))[function];
	/* Ensure that the function address is valid. */
	if (!address) {
		error_raise(E_RESOULTION, NULL);
		return 0;
	}
	/* Perform the function call internally. */
	return fmr_call(address, argc, types, arguments);
}
/* NOTE: The 'pgm_read_word' function must be used here to access the array due to the fact it is stored in flash. */
