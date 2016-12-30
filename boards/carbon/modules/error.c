#define __private_include__
#include <flipper/carbon/modules/error.h>

/* Define the virtual interface for this module. */
const struct _error error = {
	error_configure,
	error_pause,
	error_resume,
	error_raise,
	error_get,
	error_clear,
};
