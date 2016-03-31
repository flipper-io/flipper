#define __private_include__
#include <flipper/button.h>
#include <flipper/fmr.h>

void button_configure(void) {

}

uint8_t button_read(void) {

	return (uint8_t)(device.invoke(_button, _button_read, NO_ARGS) & 1);

}
