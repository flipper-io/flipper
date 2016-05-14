#define __private_include__
#include <flipper/error.h>

/* Declare all of the global variables for this module. */
uint8_t error_disclosed = 0;
lf_error_t error_code = E_OK;

/* Define the virtual interface for this module. */
const struct _error error = {
	error_configure,
	error_withold,
	error_disclose,
	error_raise,
	error_get,
	error_clear,
};