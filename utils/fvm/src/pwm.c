#include <flipper.h>

#ifdef __use_pwm__
#define __private_include__
#include <flipper/pwm.h>

int pwm_configure(void) {
	printf("Configuring the pwm.\n");
	return lf_success;
}

#endif
