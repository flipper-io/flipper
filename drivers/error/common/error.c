#define __private_include__
#include <flipper/error.h>

/* ~ Define the virtual driver object. ~ */
const struct _error error = {
	error_configure,
	error_withold,
	error_disclose,
	error_raise,
	error_get,
	error_clear,
};
