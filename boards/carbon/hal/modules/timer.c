#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/timer.h>

int timer_configure(void) {
	return lf_invoke(&_timer, _timer_configure, NULL);
}
