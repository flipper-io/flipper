#define __private_include__

#include <flipper.h>

#include <fmr/fmr.h>

void __attribute__ ((constructor)) libflipper_init() {
	
	verbose("The library 'libflipper' was configured successfully.\n\n");
	
	button_configure();
	
	flash_configure();
	
	i2c_configure();
	
	io_configure();
	
	led_configure();
	
	pwm_configure();
	
	spi_configure(0);
	
	timer_configure();
	
}