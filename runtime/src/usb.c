#include <flipper/libflipper.h>

#ifdef __use_usb__
#define __private_include__
#include <flipper/usb.h>

LF_MODULE(_usb, "usart", "Provides low level access to the device's USB bus.", _usb_id);

/* Define the virtual interface for this module. */
const struct _usb usb = {
	usb_configure
};

LF_WEAK int usb_configure(void) {
	return lf_success;
}

#endif
