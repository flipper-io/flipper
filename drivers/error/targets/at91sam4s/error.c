#define __private_include__
#include <flipper/error.h>

void error_configure(void) {

}

void error_withold(void) {

	error_disclosed = 0;

}

void error_disclose(void) {

	error_disclosed = 1;

}

void error_raise(lf_error_t code, char *format, ...) {

	/* Save the error code into the global error state. */
	error_code = code;

}

lf_error_t error_get(void) {

	return error_code;

}

void error_clear(void) {

	error_code = E_OK;

}
