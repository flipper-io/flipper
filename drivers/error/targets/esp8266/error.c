#define __private_include__
#include <flipper/error.h>
#include <flipper/platform/platform.h>

uint8_t error_disclosed;
uinterror_t error_code;

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
	/* ~ Save the error code into the global error state. ~ */
	error_code = code;
	//TODO: Use serial or something:
	(void)string;
	return;
}

void error_clear(void) {
	error_code = E_OK;
	return;
}
