#include <flipper.h>

#ifdef __use_usb__
#include <flipper/usb.h>

int usb_configure(void) {
	printf("Configuring USB.\n");
	return lf_success;
}

#endif
