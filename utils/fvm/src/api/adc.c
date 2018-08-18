#include <flipper.h>

LF_FUNC("adc") int adc_configure(void) {
	printf("Configured the adc.\n");
	return lf_success;
}
