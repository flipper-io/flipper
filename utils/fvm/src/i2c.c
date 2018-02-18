#include <flipper.h>

#ifdef __use_i2c__
#define __private_include__
#include <flipper/i2c.h>

int i2c_configure(void) {
	printf("Configuring the i2c bus.\n");
	return lf_success;
}

#endif
