#define __private_include__

#include <led/led.h>

#include <fmr/fmr.h>

void led_configure(void) {
	
	
	
}

void led_set_rgb(uint8_t r, uint8_t g, uint8_t b) {
	
	host.invoke(_led, _led_set_rgb, 3, r, g, b);
	
}