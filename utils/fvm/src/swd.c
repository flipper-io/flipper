#include <flipper.h>

#ifdef __use_swd__
#define __private_include__
#include <flipper/swd.h>

int swd_configure(void) {
	printf("Configuring the swd.\n");
	return lf_success;
}

#endif
