#define __private_include__
#include <flipper/config.h>

/* Define the virtual interface for this module. */
const struct _config config = {
	config_configure,
	config_write,
	config_read
};
