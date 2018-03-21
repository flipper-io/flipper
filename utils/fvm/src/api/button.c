#include <flipper.h>

extern struct _lf_module button;

LF_FUNC("button") int button_configure(void) {
	dyld_register(&THIS_DEVICE, &button);
	printf("Configured the button.\n");
	return lf_success;
}

LF_FUNC("button") uint8_t button_read(void) {
	printf("Reading the state of the button.\n");
	return 0;
}
