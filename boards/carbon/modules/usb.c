#define __private_include__
#include <flipper/usb.h>

#ifdef __use_usb__
/* Define the virtual interface for this module. */
const struct _usb usb = {
	usb_configure
};
#endif
