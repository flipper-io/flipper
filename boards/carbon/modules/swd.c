#define __private_include__
#include <flipper/swd.h>

#ifdef __use_swd__
/* Define the virtual interface for this module. */
const struct _swd swd = {
	swd_configure
};
#endif
