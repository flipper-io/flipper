#include <flipper.h>

#ifdef __use_pwm__
#include <flipper/pwm.h>

int pwm_configure(void) {
	printf("Configuring the pwm.\n");
	return lf_success;
}

#endif
