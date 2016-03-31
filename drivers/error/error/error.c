#define __private_include__
#include <flipper/error.h>
#include <flipper/fmr.h>

uint8_t error_disclosed = 0;
uinterror_t error_code = E_OK;

void error_configure(void) {
	return;
}

void error_withold(void) {
	error_disclosed = 0;
	return;
}

void error_disclose(void) {
	error_disclosed = 1;
	return;
}

void error_raise(uinterror_t code, char *string) {
	if(error_disclosed) {
		/* ~ Save the error code into the global error state. ~ */
		error_code = code;
	}
	else {
		fprintf(stderr, "%s\n", string);
		exit(EXIT_FAILURE);
	}
	return;
}

uinterror_t error_get(void) {

	return error_code;

}

void error_clear(void) {
	error_code = E_OK;
	return;
}
