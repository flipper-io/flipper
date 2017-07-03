#include <flipper/libflipper.h>

#ifdef __use_pwm__
#define __private_include__
#include <flipper/pwm.h>

LF_MODULE(_pwm, "pwm", "Interfaces with the device's pulse width modulation controller.", _pwm_id);

/* Define the virtual interface for this module. */
const struct _pwm pwm = {
	pwm_configure
};

LF_WEAK int pwm_configure(void) {
	return lf_invoke(&_pwm, _pwm_configure, NULL);
}

#endif
