#include <flipper.h>

#ifdef __use_dac__
#include <flipper/dac.h>

int dac_configure(void) {
	printf("Configuring the dac.\n");
	return lf_success;
}

#endif
