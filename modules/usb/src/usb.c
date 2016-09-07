#define __private_include__
#include <flipper/usb.h>

/* Define the virtual interface for this module. */
const struct _usb usb = {
	usb_configure
};
