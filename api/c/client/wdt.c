#include "libflipper.h"

enum { _wdt_fire, _wdt_configure };

void wdt_fire(struct _lf_device *device);
int wdt_configure(struct _lf_device *device);

void *wdt_interface[] = { &wdt_fire, &wdt_configure };

LF_MODULE(wdt, "wdt", wdt_interface);

LF_WEAK void wdt_fire(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "wdt", _wdt_fire, lf_void_t, &retval, NULL);
}

LF_WEAK int wdt_configure(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "wdt", _wdt_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
