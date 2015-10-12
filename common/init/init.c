#define __private_include__

#include <button/button.h>

#include <flash/flash.h>

#include <fs/fs.h>

#include <i2c/i2c.h>

#include <io/io.h>

#include <led/led.h>

#include <pwm/pwm.h>

#include <sam/sam.h>

#include <spi/spi.h>

#include <timer/timer.h>

void __attribute__ ((constructor)) libflipper_init() {
	
	button_configure();
	
	flash_configure();
	
	i2c_configure();
	
	io_configure();
	
	led_configure();
	
	pwm_configure();
	
	sam_configure();
	
	//spi_configure(0);
	
	timer_configure();
	
}