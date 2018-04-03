#include <flipper.h>

enum { _pwm_configure };

int pwm_configure(void);

void *pwm_interface[] = {
	&pwm_configure
};

LF_MODULE(pwm, "pwm", pwm_interface);

LF_WEAK int pwm_configure(void) {
	return lf_invoke(lf_get_current_device(), "pwm", _pwm_configure, lf_int_t, NULL);
}

