#define __private_include__

#include <button/button.h>

#include <fmr/fmr.h>

void button_configure(void) {
	
	
	
}

bool button_read(void) {
	
	return (little32(device.invoke(0, 1, NO_ARGS)) & 1);
	
}