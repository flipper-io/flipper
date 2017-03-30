#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/button.h>

int button_configure(void) {
	return lf_invoke(&_button, _button_configure, NULL);
}

uint8_t button_read(void) {
	return (lf_invoke(&_button, _button_read, NULL) >> 16) & 0xFF;
}
