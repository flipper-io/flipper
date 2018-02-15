#include <flipper.h>

#ifdef __use_rtc__
#define __private_include__
#include <flipper/rtc.h>

int rtc_configure(void) {
	printf("Configuring the rtc.\n");
	return lf_success;
}

#endif
