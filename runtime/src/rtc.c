#include <flipper/rtc.h>

#ifdef __use_rtc__

LF_MODULE(_rtc, "rtc", "Interfaces with the device's real time clock.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _rtc_interface rtc = {
	rtc_configure
};

LF_WEAK int rtc_configure(void) {
	return lf_success;
}

#endif
