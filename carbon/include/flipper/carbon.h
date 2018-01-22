#ifndef __carbon_h__
#define __carbon_h__

/* Flipper: Carbon Edition board header file. */
#include <flipper/libflipper.h>

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include <flipper/adc.h>
#include <flipper/button.h>
#include <flipper/dac.h>
#include <flipper/fld.h>
#include <flipper/gpio.h>
#include <flipper/i2c.h>
#include <flipper/is25lp.h>
#include <flipper/led.h>
#include <flipper/pwm.h>
#include <flipper/rtc.h>
#include <flipper/spi.h>
#include <flipper/swd.h>
#include <flipper/task.h>
#include <flipper/temp.h>
#include <flipper/timer.h>
#include <flipper/uart0.h>
#include <flipper/usart.h>
#include <flipper/usb.h>
#include <flipper/wdt.h>

#include <flipper/atsam4s/atsam4s.h>

struct _carbon_context {
	/* Device that handles interacting with the 4s. (ATMEGA16U2) */
	struct _lf_device *_u2;
	/* Microprocessor that handles code execution. (ATSAM4S16B) */
	struct _lf_device *_4s;
};

/* Attaches to all carbon devices. */
int carbon_attach(void);
/* Attaches to a carbon device over the network. */
struct _lf_device *carbon_attach_hostname(char *hostname);

#endif
