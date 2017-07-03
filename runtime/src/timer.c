#include <flipper/libflipper.h>

#ifdef __use_timer__
#define __private_include__
#include <flipper/timer.h>

LF_MODULE(_timer, "timer", "Interfaces with the device's timer peripheral.", _timer_id);

/* Define the virtual interface for this module. */
const struct _timer timer = {
	timer_configure
};

LF_WEAK int timer_configure(void) {
	return lf_invoke(&_timer, _timer_configure, NULL);
}

#endif
