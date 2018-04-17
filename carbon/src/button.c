#include <flipper.h>

enum { _button_read, _button_configure };

uint8_t button_read(void);
int button_configure(void);

void *button_interface[] = {
	&button_read,
	&button_configure
};

LF_MODULE(button, "button", button_interface);

LF_WEAK uint8_t button_read(void) {
	return lf_success;
}

LF_WEAK int button_configure(void) {
	return lf_invoke(lf_get_current_device(), "button", _button_configure, lf_int_t, NULL);
}

