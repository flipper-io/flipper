#ifndef __flipper_h__
#define __flipper_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include "core.h"

/* ~ Include the header files for user-accessible drivers. ~ */
#include <at45/at45.h>
#include <button/button.h>
#include <error/error.h>
#include <fdl/fdl.h>
#include <fmr/fmr.h>
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

/* ~ Define the flipper control type. ~ */
enum { FLIPPER_SOURCE_USB, FLIPPER_SOURCE_NETWORK, FLIPPER_SOURCE_FVM };

extern const struct _flipper {
	
	void (* configure)(void);
	void (* attach)(uint8_t source, ...);
	
} flipper;

#ifdef __private_include__

extern void flipper_configure(void);
extern void flipper_attach(uint8_t source, ...);

#endif
#endif
