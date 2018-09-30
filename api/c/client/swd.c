#include "libflipper.h"

enum { _swd_configure };

int swd_configure(struct _lf_device *device);

void *swd_interface[] = { &swd_configure };

LF_MODULE(swd, "swd", swd_interface);

LF_WEAK int swd_configure(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "swd", _swd_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
