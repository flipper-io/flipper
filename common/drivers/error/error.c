#define __private_include__

#include <error/error.h>

const struct _error error = {
	
	error_configure,
	
	error_raise,
	
	error_message
	
};