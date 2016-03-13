#define __private_include__

#include "servo.h"

#define lower_bound 800
#define upper_bound 3650

/* ~ Create a handle for the module. ~ */

static fmr_handle handle;

void servo_configure(void) {
	
	/* ~ Registers this module with the Flipper Message Runtime. Also calls the remote configure function. ~ */
	
	handle = fmr.bind(fmr_bundle_id_from_string("io.flipper.servo"));
	
}

void servo_attach(uint8_t pin) {
	
	/* ~ Invokes the function 'servo_attach' on the device. ~ */
	
	fmr.invoke(handle, _servo_attach, fmr_args(pin));
	
}

long map(float x, float in_min, float in_max, float out_min, float out_max) {
	
	return (x - in_min) * (out_max - out_min) / (in_max - in_min) + out_min;
	
}

void servo_rotate(uint32_t position) {
	
	/* ~ Invokes the function 'qux_on' on the device. ~ */
	
	long actual = map(position & 0xFF, 0, 255, lower_bound, upper_bound);
	
	fmr.invoke(handle, _servo_rotate, fmr_args(actual));
	
}