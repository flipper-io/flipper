#ifndef __flipper_h__
#define __flipper_h__

/* Include the top-level libflipper header. */
#include "libflipper.h"

/* Include the 'Flipper: Carbon Edition' device support header. */
#include "carbon.h"

/* Include the header files for all of the standard modules exposed by the toolbox. */
#include "api/adc.h"
#include "api/button.h"
#include "api/dac.h"
#include "api/gpio.h"
#include "api/i2c.h"
#include "api/led.h"
#include "api/pwm.h"
#include "api/rtc.h"
#include "api/spi.h"
#include "api/swd.h"
#include "api/temp.h"
#include "api/timer.h"
#include "api/uart0.h"
#include "api/usart.h"
#include "api/usb.h"
#include "api/wdt.h"

/* Declare the virtual interface for this driver. */
extern const struct _flipper_interface {
	/* Attaches the current instance of libflipper to the first available device over the default endpoint. */
	struct _lf_device *(* const attach)(void);
	/* Selects a previously attached Flipper device and routes all calls to it. */
	int (* const select)(struct _lf_device *device);
	/* Disconnects a previously attached Flipper device from libflipper. */
	int (* const detach)(struct _lf_device *device);
	/* Safely destroys all libflipper state before termination. */
	int (* const exit)(void);
} flipper;

struct _lf_device *flipper_attach(void);
int flipper_select(struct _lf_device *device);
int flipper_detach(struct _lf_device *device);
int flipper_exit(void);

#endif
