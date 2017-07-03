#include <flipper/libflipper.h>

#ifdef __use_rtc__
#define __private_include__
#include <flipper/rtc.h>

LF_MODULE(_rtc, "rtc", "Interfaces with the device's real time clock.", _rtc_id);

/* Define the virtual interface for this module. */
const struct _rtc rtc = {
	rtc_configure
};

LF_WEAK int rtc_configure(void) {
	return lf_success;
}

#endif
