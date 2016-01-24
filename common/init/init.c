#define __private_include__

#include <flipper/flipper.h>

void __attribute__ ((constructor)) libflipper_init() {
	
	button_configure();
	
	error_configure();
	
	i2c_configure();
	
	io_configure();
	
	led_configure();
	
	pwm_configure();
	
	sam_configure();
	
	spi_configure(0);
	
	at45_configure();
    
	timer_configure();
	
}