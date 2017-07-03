#include <flipper/libflipper.h>

#ifdef __use_adc__
#define __private_include__
#include <flipper/adc.h>

LF_MODULE(_adc, "adc", "Interfaces with the device's internal analog to digital converter.", _adc_id);

/* Define the virtual interface for this module. */
const struct _adc adc = {
	adc_configure
};

LF_WEAK int adc_configure(void) {
	return lf_success;
}

#endif
