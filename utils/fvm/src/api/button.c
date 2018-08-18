#include <flipper.h>

LF_FUNC("button") int button_configure(void) {
	printf("Configured the button.\n");
	return lf_success;
}

LF_FUNC("button") uint8_t button_read(void) {
	printf("Reading the state of the button.\n");
	return 0;
}
