#define __private_include__
#include <error/error.h>
#include <fmr/fmr.h>

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

void error_raise(uinterror_t code, unsigned char *string) {
	if(error.disclosed) {
		/* ~ Save the error code into the global error state. ~ */
		*((uinterror_t *)(&error.code)) = code;
	}
	else {
		error(string);
		exit(EXIT_FAILURE);
	}
	return;
}

void error_clear(void) {
	*((uinterror_t *)(&error.code)) = E_OK;
	return;
}
