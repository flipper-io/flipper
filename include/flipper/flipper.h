/* flipper.h - Provides an interface for attaching and detaching devices. */

#ifndef __flipper_h__
#define __flipper_h__

/* ~ Include all types and macros exposed by the Flipper Toolbox. ~ */
#include <flipper/core.h>

/* Include all supporting header files. */
#include <flipper/fmr.h>
#include <flipper/error.h>

/* ~ Declare the virtual interface for this driver. ~ */
extern struct _flipper {
	/* Attaches the current instance of libflipper to the first available device over the default endpoint. */
	int (* attach)(void);
	/* Attaches to a Flipper device by name over the default endpoint. */
	int (* attach_usb)(char *name);
	/* Attaches to a Flipper device by name and IP over the network. */
	int (* attach_network)(char *name, char *ip);
	/* Attaches to a Flipper device by name over an arbitrary endpoint. */
	int (* attach_endpoint)(char *name, const struct _lf_endpoint *endpoint);
	/* Selects a Flipper device. */
	int (* select)(char *name);
	/* Detaches a Flipper device. */
	int (* detach)(char *name);
	/* Safely destroys all libflipper state before termination. */
	int (* exit)(void);
	/* The head of a linked list that aggregates all attached devices. */
	struct _lf_device *attached;
	/* The selected device with which interaction will take place. */
	struct _lf_device *device;
} flipper;

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */

int flipper_attach(void);
int flipper_attach_usb(char *name);
int flipper_attach_network(char *name, char *hostname);
int flipper_attach_endpoint(char *name, const struct _lf_endpoint *endpoint);
int flipper_select(char *name);
int flipper_detach(char *name);
int flipper_exit(void);

/* ~ User functions. ~ */

/* Performs a proper function invocation on the device associated with the provided module. */
int lf_invoke(struct _fmr_module *module, fmr_function function, struct _fmr_list *args);

/* ~ Helper functions. ~ */

/* Gets the word size of the specified device. */
fmr_type lf_word_size(struct _lf_device *device);
/* Sends a packet to the specified device. */
int lf_transfer_packet(struct _lf_device *device, struct _fmr_packet *packet);
/* Retrieves a packet from the specified device. */
int lf_retrieve_packet(struct _lf_device *device, struct _fmr_packet *packet);
/* Obtains a packet from the specified device and parses the result of the previous operation. */
int lf_obtain_result(struct _lf_device *device, struct _fmr_result *result);

#endif
#endif
