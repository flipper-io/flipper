#include <flipper.h>

enum { _rtc_configure };

int rtc_configure(void);

void *rtc_interface[] = {
	&rtc_configure
};

LF_MODULE(rtc, "rtc", rtc_interface);

LF_WEAK int rtc_configure(void) {
	return lf_invoke(lf_get_current_device(), "rtc", _rtc_configure, lf_int_t, NULL);
}

