#define __private_include__

#include <button/button.h>

#include <led/led.h>

void button_configure(void) {
	
	led.rgb(25, 0, 0);
	
}

bool button_read(void) {
	
	
	
	return 0xDF;
	
}