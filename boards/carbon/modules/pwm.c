#define __private_include__
#include <flipper/carbon/modules/pwm.h>

#ifdef __use_pwm__
/* Define the virtual interface for this module. */
const struct _pwm pwm = {
	pwm_configure
};
#endif
