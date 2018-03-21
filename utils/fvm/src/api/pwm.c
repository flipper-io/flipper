#include <flipper.h>

extern struct _lf_module pwm;

LF_FUNC("pwm") int pwm_configure(void) {
	dyld_register(&THIS_DEVICE, &pwm);
	printf("Configured the pwm.\n");
	return lf_success;
}
