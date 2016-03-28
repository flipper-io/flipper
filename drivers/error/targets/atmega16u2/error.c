#define __private_include__
#include <flipper/error/error.h>
#include <flipper/platform/platform.h>

void error_configure(void) {
	return;
}

void error_withold(void) {
	*((uint8_t *)(&error.disclosed)) = 0;
	return;
}

void error_disclose(void) {
	*((uint8_t *)(&error.disclosed)) = 1;
	return;
}

void error_raise(uinterror_t code, char *string) {
	/* ~ Save the error code into the global error state. ~ */
	*((uinterror_t *)(&error.code)) = code;
	//TODO: Use serial or something:
	(void)string;
	return;
}

void error_clear(void) {
	*((uinterror_t *)(&error.code)) = E_OK;
	return;
}
