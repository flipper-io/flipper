#ifndef __flipper_h__
#define __flipper_h__

/* ~ Include the header file that exposes the API for user-accessible drivers. ~ */
#include <flipper/drivers.h>

/* ~ Define the flipper control type. ~ */
typedef enum { FLIPPER_USB, FLIPPER_NETWORK, FLIPPER_FVM } lf_endpoint;

struct _lf_device {

	/* ~ The name of the attached device. ~ */
	char *name;

	/* ~ The hardware identifier for the device. ~ */
	uintcrc_t identifier;

	/* ~ The endpoint via which this device is attached. This also provides
	 *   the appropriate interpretation for the handle field. ~ */
	lf_endpoint endpoint;

	/* ~ References the endpoint specific descriptor from which the device can be accessed. ~ */
	void *handle;

	/* ~ The next attached device. ~ */
	struct _lf_device *next;
	
};

extern struct _flipper {

	const int (* attach)(lf_endpoint endpoint, char *name);

	const int (* detach)(char *name);

	const int (* select)(char *name);

	/* ~ Points to the device to which the current instance of libflipper is attached. ~ */
	struct _lf_device *device;

	/* ~ Points to a head of a linked list representing the attached devices. ~ */
	struct _lf_device *devices;

} flipper;

#ifdef __private_include__

extern int flipper_select(char *name);
extern int flipper_attach(lf_endpoint endpoint, char *name);
extern int flipper_detach(char *name);

#endif
#endif
