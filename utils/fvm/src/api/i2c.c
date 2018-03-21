
#include <flipper.h>

extern struct _lf_module i2c;

LF_FUNC("i2c") int i2c_configure(void) {
	dyld_register(&THIS_DEVICE, &i2c);
	printf("Configured the i2c bus.\n");
	return lf_success;
}
