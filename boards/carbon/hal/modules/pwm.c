#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_pwm, "pwm", "Interfaces with the device's pulse width modulation controller.", _pwm_id);

int pwm_configure(void) {
	return lf_invoke(&_pwm, _pwm_configure, NULL);
}
