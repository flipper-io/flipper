#define __private_include__
#include <flipper/fmr.h>
#include <flipper/modules.h>
#include <platform/atsam4s16b.h>

/* Define the standard modules accessible on this platform. */
const void *const fmr_modules[] = {
	NULL,    // adc
	NULL,    // button
	NULL,    // cpu
	NULL,    // dac
	NULL,    // error
	NULL,    // fmr
	NULL,    // fs
	&gpio,   // gpio
	NULL,    // i2c
	NULL,    // led
	NULL,    // pwm
	NULL,    // rtc
	NULL,    // spi
	NULL,    // swd
	NULL,    // temp
	NULL,    // timer
	&uart0,  // usart0
	&usart,	 // usart
	NULL,    // usb
	NULL,    // wdt
};

/* Executes a standard module. */
fmr_return fmr_execute(fmr_module module, fmr_function function, fmr_argc argc, fmr_types types, void *arguments) {
	/* Dereference the pointer to the target module. */
	void *object = (void *)(fmr_modules[module]);
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
