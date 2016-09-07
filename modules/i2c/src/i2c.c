#define __private_include__
#include <flipper/i2c.h>

/* Define the virtual interface for this module. */
const struct _i2c i2c = {
	i2c_configure
};
