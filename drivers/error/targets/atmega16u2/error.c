#define __private_include__
#include <error/error.h>
#include <platform.h>

void error_configure(void) {

}


void error_raise(uint16_t id) {

	*(uinterror_t *)(&error.code) = id;
	
}

char *error_message(void) {

	return "";

}