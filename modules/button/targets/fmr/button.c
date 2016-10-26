#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_button, "button", "Interacts with the onboard button.", _button_id);

int button_configure(void) {
	return lf_invoke(&_button, _button_configure, NULL);
}

uint8_t button_read(void) {
	return lf_invoke(&_button, _button_read, NULL);
}
