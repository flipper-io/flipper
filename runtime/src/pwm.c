#define __private_include__
#include <flipper/pwm.h>

#ifdef __use_pwm__

LF_MODULE(_pwm, "pwm", "Interfaces with the device's pulse width modulation controller.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _pwm pwm = {
	pwm_configure
};

LF_WEAK int pwm_configure(void) {
	return lf_invoke(&_pwm, _pwm_configure, fmr_int_t, NULL);
}

#endif
