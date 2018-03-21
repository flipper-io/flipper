#include <flipper.h>

extern struct _lf_module swd;

LF_FUNC("swd") int swd_configure(void) {
	dyld_register(&THIS_DEVICE, &swd);
	printf("Configured the swd.\n");
	return lf_success;
}
