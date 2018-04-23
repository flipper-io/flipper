#include <flipper.h>

enum { _adc_configure };

int adc_configure(void);

void *adc_interface[] = {
	&adc_configure
};

LF_MODULE(adc, "adc", adc_interface);

LF_WEAK int adc_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "adc", _adc_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

