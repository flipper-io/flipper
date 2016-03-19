#define __private_include__
#include <error/error.h>
#include <fmr/fmr.h>

void error_configure(void) {

}


void error_raise(uint16_t id) {

	/* ~ Save the error code into the global error state. ~ */
	*(uinterror_t *)(&error.code) = id;
	
	if (error.code) printf("\nGot error code: %i\n\n", id);
	
}

char *error_message(void) {

	return "";

}