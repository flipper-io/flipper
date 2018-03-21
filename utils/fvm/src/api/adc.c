#include <flipper.h>

extern struct _lf_module adc;

int adc_configure(void) {
	dyld_register(&THIS_DEVICE, &adc);
	printf("Configured the adc.\n");
	return lf_success;
}
