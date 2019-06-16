#include "libflipper.h"

enum { _usb_configure };

int usb_configure(void);

void *usb_interface[] = { &usb_configure };

LF_MODULE(usb);

LF_WEAK int usb_configure(void) {
    lf_return_t retval;
    lf_invoke(lf_get_selected(), "usb", _usb_configure, lf_int_t, &retval, NULL);
    return (int)retval;
}
