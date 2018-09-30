#include "libflipper.h"

enum { _rtc_configure };

int rtc_configure(struct _lf_device *device);

void *rtc_interface[] = { &rtc_configure };

LF_MODULE(rtc, "rtc", rtc_interface);

LF_WEAK int rtc_configure(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "rtc", _rtc_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
