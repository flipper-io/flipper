#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/pwm.h>

int pwm_configure(void) {
	return lf_invoke(&_pwm, _pwm_configure, NULL);
}
