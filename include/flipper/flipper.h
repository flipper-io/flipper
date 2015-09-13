#ifndef flipper_h

#define flipper_h

/* ~ Include the types header. ~ */

#include <flipper/types.h>

/* ~ Include the header files for user-accessable drivers. ~ */

#include <drivers/button.h>

#include <drivers/flash.h>

#include <drivers/fs.h>

#include <drivers/i2c.h>

#include <drivers/io.h>

#include <drivers/led.h>

#include <drivers/pwm.h>

#include <drivers/spi.h>

#include <drivers/timer.h>

#include <drivers/usart.h>

#include <drivers/usb.h>

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