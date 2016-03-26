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

#include <pthread.h>

/* ~ Define the flipper control type. ~ */
typedef enum { FLIPPER_USB, FLIPPER_NETWORK, FLIPPER_FVM } lf_endpoint;

struct _lf_device {

	/* ~ The name of the attached device. ~ */
	char *name;

	/* ~ The hardware identifier for the device. ~ */
	uintcrc_t identifier;

	/* ~ References the endpoint specific descriptor from which the device can be accessed. ~ */
	void *handle;

	/* ~ The next attached device. ~ */
	struct _lf_device *next;
	
};

extern struct _flipper {

	int (* attach)(lf_endpoint endpoint, char *name);

	int (* select)(char *name);

	/* ~ Points to the device to which the current instance of libflipper is attached. ~ */
	struct _lf_device *device;

	/* ~ Points to a head of a linked list representing the attached devices. ~ */
	struct _lf_device *devices;

	/* ~ All libflipper API calls are serialized with this mutex. ~ */
	pthread_mutex_t lock;

} flipper;

#ifdef __private_include__

extern int flipper_select(char *name);
extern int flipper_attach(lf_endpoint endpoint, char *name);

#endif
#endif
