#ifndef flipper_h

#define flipper_h

/* ~ Include the types header. ~ */

#include <types.h>

/* ~ Include the header files for user-accessable drivers. ~ */

#include <button/button.h>

#include <flash/flash.h>

#include <fs/fs.h>

#include <i2c/i2c.h>

#include <io/io.h>

#include <led/led.h>

#include <pwm/pwm.h>

#include <spi/spi.h>

#include <timer/timer.h>

#include <usart/usart.h>

#include <usb/usb.h>

/* ~ Define the flippper control type. ~ */

enum { FLIPPER_SOURCE_USB, FLIPPER_SOURCE_NETWORK };

enum { _flipper_configure };

extern const struct _flipper {
	
	void (* configure)(void);
	
	void (* attach)(uint8_t source, ...);
	
} flipper;

#ifdef __private_include__

extern void flipper_configure(void);

extern void flipper_attach(uint8_t source, ...);

#endif

#endif