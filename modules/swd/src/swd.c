#define __private_include__
#include <flipper/swd.h>

/* Define the virtual interface for this module. */
const struct _swd swd = {
	swd_configure
};
