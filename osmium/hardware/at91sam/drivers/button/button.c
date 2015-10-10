#define __private_include__

#include <button/button.h>

extern void pio_interrupt(void);

void button_configure(void) {
	
	pio_interrupt();
	
}

bool button_read(void) {
	
	return false;
	
}