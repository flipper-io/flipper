#include "libflipper.h"

enum { _timer_register, _timer_configure };

int timer_register(struct _lf_device *device, uint32_t ticks, void* callback);
int timer_configure(struct _lf_device *device);

void* timer_interface[] = { &timer_register, &timer_configure };

LF_MODULE(timer, "timer", timer_interface);

LF_WEAK int timer_register(struct _lf_device *device, uint32_t ticks, void* callback) {
    lf_return_t retval;
    lf_invoke(device, "timer", _timer_register, lf_int_t, &retval,
              lf_args(lf_infer(ticks), lf_infer(callback)));
    return (int)retval;
}

LF_WEAK int timer_configure(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "timer", _timer_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
