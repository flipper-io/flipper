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
	struct _lf_device *(* attach)(void);
	/* Attaches to a Flipper device by name over the USB endpoint. */
	struct _lf_device *(* attach_usb)(char *name);
	/* Attaches to a Flipper device by name and hostname/IP over the network endpoint. */
	struct _lf_device *(* attach_network)(char *name, char *hostname);
	/* Attaches to a Flipper device by name over an arbitrary endpoint. */
	struct _lf_device *(* attach_endpoint)(char *name, struct _lf_endpoint *endpoint);
	/* Selects a previously attached Flipper device and routes all calls to it. */
	int (* select)(struct _lf_device *device);
	/* Disconnects a previously attached Flipper device from libflipper. */
	int (* detach)(struct _lf_device *device);
	/* Safely destroys all libflipper state before termination. */
	int (* exit)(void);
	/* Stores the last observed error code. */
	lf_error_t error_code;
	/* Global flag that indicates whether or not error_raise() should print to stderr and call exit(). */
	uint8_t errors_cause_side_effects;
	/* Points to the actively selected device with which interaction will take place. */
	struct _lf_device *device;
} flipper;

#ifdef __private_include__

/* ~ Declare the prototypes for all functions exposed by this driver. ~ */
extern struct _lf_device *flipper_attach(void);
extern struct _lf_device *flipper_attach_usb(char *name);
extern struct _lf_device *flipper_attach_network(char *name, char *hostname);
extern struct _lf_device *flipper_attach_endpoint(char *name, struct _lf_endpoint *endpoint);
extern int flipper_select(struct _lf_device *device);
extern int flipper_detach(struct _lf_device *device);
extern int flipper_exit(void);

/* ~ User functions. ~ */

/* Performs a proper function invocation on the device associated with the provided module. */

/**
 * @brief Invokes a function in a module with a list of parameters.
 *
 * @param module The module in which the function resides.
 * @param function The function to be invoked.
 * @param paramters The list of parameters to be passed to the function.
 *
 */
extern fmr_return lf_invoke(struct _lf_module *module, fmr_function function, struct _fmr_list *parameters);
/* Moves data from the address space of the host to that of the device. */
extern int lf_push(struct _lf_module *module, fmr_function function, void *source, lf_size_t length, struct _fmr_list *parameters);
/* Moves data from the address space of the device to that of the host. */
extern int lf_pull(struct _lf_module *module, fmr_function function, void *destination, lf_size_t length, struct _fmr_list *parameters);
/* Binds the current instance of an 'lf_module' to its counterpart on the device. */
extern int lf_bind(struct _lf_module *module);


/* ~ Helper functions. ~ */

/* Gets the word size of the specified device. */
extern fmr_type lf_word_size(struct _lf_device *device);
/* Load the device's configuration information. */
int lf_load_configuration(struct _lf_device *device);

/* Obtains a result from a device. */
int lf_get_result(struct _lf_device *device, struct _fmr_result *result);
/* Sends a packet to the specified device. */
extern int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet);
/* Retrieves a packet from the specified device. */
extern int lf_retrieve(struct _lf_device *device, struct _fmr_result *response);

/* Prints verbose information about the packet disassembly. */
extern void lf_debug_packet(struct _fmr_packet *packet, size_t length);

#endif
#endif
