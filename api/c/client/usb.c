#include "libflipper.h"

enum { _usb_configure };

int usb_configure(struct _lf_device *device);

void *usb_interface[] = { &usb_configure };

LF_MODULE(usb, "usb", usb_interface);

LF_WEAK int usb_configure(struct _lf_device *device) {
    lf_return_t retval;
    lf_invoke(device, "usb", _usb_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
