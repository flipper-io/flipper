#include <flipper.h>

LF_FUNC("timer") int timer_configure(void) {
	printf("Configured the timer.\n");
	return lf_success;
}
