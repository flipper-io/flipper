#include <flipper/button.h>

#ifdef __use_button__

LF_MODULE(_button, "button", "Interacts with the onboard button.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _button_interface button = {
	button_configure,
	button_read
};

LF_WEAK int button_configure(void) {
	return lf_invoke(lf_get_current_device(), &_button, _button_configure, lf_int_t, NULL);
}

LF_WEAK uint8_t button_read(void) {
	return lf_invoke(lf_get_current_device(), &_button, _button_read, lf_int8_t, NULL);
}

#endif
