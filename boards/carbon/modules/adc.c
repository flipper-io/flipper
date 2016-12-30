#define __private_include__
#include <flipper/carbon/adc.h>

#ifdef __use_adc__
/* Define the virtual interface for this module. */
const struct _adc adc = {
	adc_configure
};
#endif
