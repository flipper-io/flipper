#include "libflipper.h"

enum { _pwm_configure };

int pwm_configure(struct _lf_device *device);

void *pwm_interface[] = { &pwm_configure };

LF_MODULE(pwm, "pwm", pwm_interface);

LF_WEAK int pwm_configure(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "pwm", _pwm_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
