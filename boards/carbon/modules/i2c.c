#define __private_include__
#include <flipper/carbon/modules/i2c.h>

#ifdef __use_i2c__
/* Define the virtual interface for this module. */
const struct _i2c i2c = {
	i2c_configure
};
#endif
