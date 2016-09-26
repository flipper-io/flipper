#define __private_include__
#include <flipper/error.h>
#include <platform/atsam4s16b.h>

void error_raise(lf_error_t error, const char *format, ...) {

}

lf_error_t error_get(void) {
	return E_OK;
}

void error_clear(void) {

}

void error_resume(void) {

}

void error_pause(void) {

}
