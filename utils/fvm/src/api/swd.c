#include <flipper.h>

LF_FUNC("swd") int swd_configure(void) {
	printf("Configured the swd.\n");
	return lf_success;
}
