#define __private_include__
#include <flipper/error/error.h>

/* ~ Define the virtual driver object. ~ */
struct _error error = {

	error_configure,
	error_withold,
	error_disclose,
	error_raise,
	error_clear,
	0,
	E_OK

};
