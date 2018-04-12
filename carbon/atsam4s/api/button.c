#include <flipper/button.h>

LF_FUNC("button") int button_configure(void) {
	return lf_success;
}

LF_FUNC("button") uint8_t button_read(void) {
	printf("Reading button value.\n");
	return false;
}
