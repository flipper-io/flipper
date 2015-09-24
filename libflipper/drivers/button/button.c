#define __private_include__

#include <button/button.h>

#include <fmr/fmr.h>

void button_configure(void) {
	
	
	
}

bool button_read(void) {
	
	return (bool)(host.invoke(_button, _button_read, NO_ARGS));
	
}