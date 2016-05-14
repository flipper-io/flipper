#define __private_include__
#include <flipper/fdl.h>

/* Define the virtual interface for this module. */
const struct _fdl fdl = {
	fdl_configure,
	fdl_load,
	fdl_launch,
	fdl_resolve
};
