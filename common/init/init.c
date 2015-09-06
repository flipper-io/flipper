#define __private_include__

#include <flipper/flipper.h>

#include <fmr/fmr.h>

void __attribute__ ((constructor)) libflipper_init() {
	
	verbose("The library 'libflipper' was configured successfully.\n\n");
	
	usb_configure(0);
	
	button_configure();
	
	flash_configure();
	
	host_configure();
	
	self_configure();
	
	device_configure();
	
	io_configure();
	
	led_configure();
	
	pwm_configure();
	
	spi_configure(0);
	
	timer_configure();
	
	usart0_configure(0);
	
}