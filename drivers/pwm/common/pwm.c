#define __private_include__
#include <flipper/pwm.h>

/* Define the virtual interface for this module. */
const struct _pwm pwm = {
	pwm_configure
};
