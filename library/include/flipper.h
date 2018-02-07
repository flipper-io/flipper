#ifndef __flipper_h__
#define __flipper_h__

/* Include the top-level libflipper header. */
#include <flipper/libflipper.h>
/* Include the 'Flipper: Carbon Edition' device support header. */
#include <flipper/carbon.h>

/* ~ Declare the virtual interface for this driver. ~ */
extern const struct _flipper {
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
