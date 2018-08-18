#include <flipper.h>

LF_FUNC("pwm") int pwm_configure(void) {
	printf("Configured the pwm.\n");
	return lf_success;
}
