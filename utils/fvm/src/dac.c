#include <flipper.h>

#ifdef __use_dac__
#define __private_include__
#include <flipper/dac.h>

int dac_configure(void) {
	printf("Configuring the dac.\n");
	return lf_success;
}

#endif
