#include <flipper.h>

extern struct _lf_module dac;

LF_FUNC("dac") int dac_configure(void) {
	dyld_register(&THIS_DEVICE, &dac);
	printf("Configured the dac.\n");
	return lf_success;
}
