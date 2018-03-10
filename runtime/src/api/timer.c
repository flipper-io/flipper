#include <flipper/timer.h>

#ifdef __use_timer__

LF_MODULE(_timer, "timer", "Interfaces with the device's timer peripheral.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _timer_interface timer = {
	timer_configure
};

LF_WEAK int timer_configure(void) {
	return lf_invoke(lf_get_current_device(), &_timer, _timer_configure, lf_int_t, NULL);
}

#endif
