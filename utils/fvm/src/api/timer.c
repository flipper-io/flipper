#include <flipper.h>

extern struct _lf_module timer;

int timer_configure(void) {
	dyld_register(&THIS_DEVICE, &timer);
	printf("Configured the timer.\n");
	return lf_success;
}
