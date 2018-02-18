#include <flipper.h>

#ifdef __use_timer__
#include <flipper/timer.h>

int timer_configure(void) {
	printf("Configuring the timer.\n");
	return lf_success;
}

#endif
