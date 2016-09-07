#define __private_include__
#include <flipper/adc.h>

/* Define the virtual interface for this module. */
const struct _adc adc = {
	adc_configure
};
