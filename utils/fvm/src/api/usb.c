#include <flipper.h>

LF_FUNC("usb") int usb_configure(void) {
	printf("Configured USB.\n");
	return lf_success;
}
