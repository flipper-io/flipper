#define __private_include__
#include <flipper/carbon/modules/dac.h>

#ifdef __use_dac__
/* Define the virtual interface for this module. */
const struct _dac dac = {
	dac_configure
};
#endif
