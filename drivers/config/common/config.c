#define __private_include__
#include <flipper/config.h>

/* ~ Define the virtual driver object. ~ */
const struct _config config = {

	config_configure,
	config_write,
	config_read

};