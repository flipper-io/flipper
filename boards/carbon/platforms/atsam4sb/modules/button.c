#define __private_include__
#include <flipper/carbon/button.h>
#include <flipper/carbon/platforms/atsam4s16b.h>

int button_configure(void) {
	return lf_success;
}

uint8_t button_read(void) {

	return false;
}
