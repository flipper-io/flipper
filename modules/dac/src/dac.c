#define __private_include__
#include <flipper/dac.h>

/* Define the virtual interface for this module. */
const struct _dac dac = {
	dac_configure
};
