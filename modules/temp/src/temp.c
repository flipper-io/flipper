#define __private_include__
#include <flipper/temp.h>

#ifdef __use_temp__
/* Define the virtual interface for this module. */
const struct _temp temp = {
	temp_configure
};
#endif
