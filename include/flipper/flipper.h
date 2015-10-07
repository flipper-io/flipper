#ifndef flipper_h

#define flipper_h

/* ~ Include the types header. ~ */

#include <flipper/types.h>

/* ~ Include the header files for user-accessible drivers. ~ */

#include <flipper/button/button.h>

#include <flipper/flash/flash.h>

#include <flipper/fs/fs.h>

#include <flipper/i2c/i2c.h>

#include <flipper/io/io.h>

#include <flipper/led/led.h>

#include <flipper/pwm/pwm.h>

#include <flipper/sam/sam.h>

#include <flipper/spi/spi.h>

#include <flipper/timer/timer.h>

#include <flipper/usart/usart.h>

#include <flipper/usb/usb.h>

#include <flipper/wifi/wifi.h>

/* ~ Define the flipper control type. ~ */

enum { FLIPPER_SOURCE_USB, FLIPPER_SOURCE_NETWORK, FLIPPER_SOURCE_FVM };

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
