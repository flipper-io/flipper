#include <flipper.h>

LF_FUNC("i2c") int i2c_configure(void) {
	printf("Configured the i2c bus.\n");
	return lf_success;
}
