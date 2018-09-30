#include "libflipper.h"

enum { _button_read, _button_configure };

uint8_t button_read(struct _lf_device *device);
int button_configure(struct _lf_device *device);

void *button_interface[] = { &button_read, &button_configure };

LF_MODULE(button, "button", button_interface);

LF_WEAK uint8_t button_read(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "button", _button_read, lf_int8_t, &retval, NULL);
    return (uint8_t)retval;
}

LF_WEAK int button_configure(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "button", _button_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
