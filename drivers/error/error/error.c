#define __private_include__
#include <flipper/error/error.h>
#include <flipper/fmr/fmr.h>

void error_configure(void) {
	return;
}

void error_withold(void) {
	error.disclosed = 0;
	return;
}

void error_disclose(void) {
	error.disclosed = 1;
	return;
}

void error_raise(uinterror_t code, char *string) {
	if(error.disclosed) {
		/* ~ Save the error code into the global error state. ~ */
		error.code = code;
	}
	else {
		fprintf(stderr, "%s\n", string);
		exit(EXIT_FAILURE);
	}
	return;
}

void error_clear(void) {
	error.code = E_OK;
	return;
}
