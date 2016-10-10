#define __private_include__
#include <flipper/fmr.h>
#include <flipper/modules.h>
#include <platform/atsam4s16b.h>

/* Define the standard modules accessible on this platform. */
const void *const fmr_modules[] = {
	_forward_id,    // adc
	_forward_id,    // button
	_forward_id,    // cpu
	_forward_id,    // dac
	_forward_id,    // error
	_forward_id,    // fmr
	_forward_id,    // fs
	&gpio,    		// gpio
	_forward_id,    // i2c
	_forward_id,    // led
	_forward_id,    // pwm
	_forward_id,    // rtc
	_forward_id,    // spi
	_forward_id,    // swd
	_forward_id,    // temp
	_forward_id,    // timer
	&uart0,         // usart0
	_forward_id,    // usart
	_forward_id,    // usb
	_forward_id,    // wdt
};

const void *lf_std_function(fmr_module module, fmr_function function) {
	/* Dereference the pointer to the target module. */
	void *object = (void *)(fmr_modules[module]);
	/* Dereference and return a pointer to the target function. */
	return ((void **)(object))[function];
}
