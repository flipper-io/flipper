#include <flipper/usb.h>

#ifdef __use_usb__

LF_MODULE(_usb, "usart", "Provides low level access to the device's USB bus.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _usb_interface usb = {
	usb_configure
};

LF_WEAK int usb_configure(void) {
	return lf_success;
}

#endif
