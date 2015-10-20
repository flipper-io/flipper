#define __private_include__

#include <button/button.h>

#include <usart/usart.h>

#include <flash/flash.h>

extern bool state;

void button_configure(void) {
	
	
	
}

bool button_read(void) {
	
	char buf[11];
	
	flash_pull(buf, 11, 0x1234);
	
	usart1.push(buf, 11);
	
	return false;
	
}