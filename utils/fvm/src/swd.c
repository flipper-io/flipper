#include <flipper.h>

#ifdef __use_swd__
#include <flipper/swd.h>

int swd_configure(void) {
	printf("Configuring the swd.\n");
	return lf_success;
}

#endif
