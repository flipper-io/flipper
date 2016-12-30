#define __private_include__
#include <flipper/error.h>
#include <flipper/carbon/platforms/atsam4s16b.h>

int error_configure(void) {
	return lf_success;
}

void error_raise(lf_error_t error, const char *format, ...) {
	lf_self.error = error;
}

lf_error_t error_get(void) {
	return lf_self.error;
}

void error_clear(void) {
	lf_self.error = E_OK;
}

void error_resume(void) {

}

void error_pause(void) {

}
