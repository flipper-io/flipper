#include <flipper.h>

LF_FUNC("dac") int dac_configure(void) {
	printf("Configured the dac.\n");
	return lf_success;
}
