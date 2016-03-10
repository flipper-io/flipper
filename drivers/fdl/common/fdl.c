#define __private_include__
#include <fdl/fdl.h>

/* ~ Define the virtual driver object. ~ */
const struct _fdl fdl = {

	fdl_configure,
	fdl_load,
	fdl_launch,
	fdl_resolve

};