#define __private_include__
#include <flipper/button.h>
#include <flipper/fmr.h>

void button_configure(void) {

}

bool button_read(void) {

	return (bool)(device.invoke(_button, _button_read, NO_ARGS) & 1);

}
