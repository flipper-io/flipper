#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

#if 0

void __attribute__ ((constructor)) libflipper_init() {
	
	verbose("The library 'libflipper' was configured successfully.\n\n");
	
	usb_configure(0);
	
	button_configure();
	
	flash_configure();
	
	io_configure();
	
	led_configure();
	
	self_configure();
	
	pwm_configure();
	
	spi_configure(0);
	
	timer_configure();
	
}

#endif