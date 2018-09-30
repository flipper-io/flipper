#include "libflipper.h"

enum { _dac_configure };

int dac_configure(struct _lf_device *device);

void *dac_interface[] = { &dac_configure };

LF_MODULE(dac, "dac", dac_interface);

LF_WEAK int dac_configure(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "dac", _dac_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
