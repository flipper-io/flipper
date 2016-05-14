#ifndef __flipper_h__
#define __flipper_h__

/* Include the header file that exposes the API for user-accessible drivers. */
#include <flipper/drivers.h>

/* Define the flipper control type. */
typedef enum { FLIPPER_USB, FLIPPER_NETWORK, FLIPPER_FVM } lf_endpoint;

struct _lf_device {

	/* The name of the attached device. */
	char *name;

	/* The hardware identifier for the device. */
	uintcrc_t identifier;

	/* The endpoint via which this device is attached. This also provides
	 *   the appropriate interpretation for the handle field. */
	lf_endpoint endpoint;

	/* References the endpoint specific descriptor from which the device can be accessed. */
	void *handle;

	/* The next attached device. */
	struct _lf_device *next;
	
};

const extern struct _flipper {

	int (* attach)(void);

	int (* attach_name)(lf_endpoint endpoint, char *name);

	int (* detach)(char *name);

	int (* select)(char *name);

} flipper;

#ifdef __private_include__

int flipper_attach(void);
int flipper_attach_name(lf_endpoint endpoint, char *name);
int flipper_select(char *name);
int flipper_detach(char *name);

extern struct _lf_device *flipper_device;
extern struct _lf_device *flipper_devices;

#endif
#endif
