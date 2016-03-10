#define __private_include__
#include <error/error.h>

/* ~ Define the virtual driver object. ~ */
const struct _error error = {

	error_configure,
	error_raise,
	error_message

};