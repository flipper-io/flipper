#include <flipper/libflipper.h>

#ifdef __use_button__
#define __private_include__
#include <flipper/button.h>

LF_MODULE(_button, "button", "Interacts with the onboard button.");

/* Define the virtual interface for this module. */
const struct _button button = {
	button_configure,
	button_read
};

LF_WEAK int button_configure(void) {
	return lf_invoke(&_button, _button_configure, NULL);
}

LF_WEAK uint8_t button_read(void) {
	return (lf_invoke(&_button, _button_read, NULL) >> 16) & 0xFF;
}

#endif
