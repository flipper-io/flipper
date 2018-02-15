#include <flipper.h>

#ifdef __use_adc__
#define __private_include__
#include <flipper/adc.h>

int adc_configure(void) {
	printf("Configuring the adc.\n");
	return lf_success;
}

#endif
