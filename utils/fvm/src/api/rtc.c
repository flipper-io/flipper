#include <flipper.h>

LF_FUNC("rtc") int rtc_configure(void) {
	printf("Configured the rtc.\n");
	return lf_success;
}
