#define __private_include__

#include <button/button.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

void button_configure(void) {
	
	
	
}

bool button_read(void) {
	
	return (bool)(device.invoke(_button, _button_read, NO_ARGS));
	
}