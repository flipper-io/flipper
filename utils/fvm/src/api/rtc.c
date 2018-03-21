
#include <flipper.h>

extern struct _lf_module rtc;

LF_FUNC("rtc") int rtc_configure(void) {
	dyld_register(&THIS_DEVICE, &rtc);
	printf("Configured the rtc.\n");
	return lf_success;
}
