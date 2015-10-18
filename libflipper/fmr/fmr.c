#define __private_include__

#include <button/button.h>

#include <flash/flash.h>

#include <fmr/fmr.h>

#include <platform/fmr.h>

#include <fs/fs.h>

#include <i2c/i2c.h>

#include <io/io.h>

#include <led/led.h>

#include <pwm/pwm.h>

#include <sam/sam.h>

#include <spi/spi.h>

#include <timer/timer.h>

#include <usart/usart.h>

#include <usb/usb.h>

#include <wifi/wifi.h>

#include <fs/crc.h>

const void * const objects[] = { &button, &flash, &self, &host, &device, &fs, &i2c, &io, &led, &pwm, &sam, &spi, &timer, &usart, &usart1, &dbgu, &usb, &wifi };

uint32_t internal_call(void *function, uint8_t argc, void *argv) {
	
	return 0;
	
}