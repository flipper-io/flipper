#define __private_include__
#include <flipper/button/button.h>
#include <flipper/fmr/fmr.h>

void button_configure(void) {

}

bool button_read(void) {

	return device.invoke(_button, _button_read, NO_ARGS) & 1;

}
