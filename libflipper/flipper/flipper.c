#define __private_include__
#include <flipper/flipper.h>
#include <flipper/fmr.h>
#include <flipper/usb.h>
#include <flipper/platform/network.h>
#include <flipper/platform/platform.h>

struct _flipper flipper = {
	(const int (*)(lf_endpoint endpoint, char *name))flipper_attach,
	(const int (*)(char *name))flipper_detach,
	(const int (*)(char *name))flipper_select,
	NULL,
	NULL,
};

/* ~ Obtain a device and endpoint for a given device name. ~ */
struct _lf_device *lf_obtain_device(char *name) {

	struct _lf_device *current = flipper.devices;

	while (current) {

		/* ~ See if we have a match. ~ */
		if (!strcmp(current -> name, name)) {
			return current;
		}

		/* ~ If we don't, advance to the next attached device. ~ */
		current = current -> next;

	}

	return NULL;

}

/* ~ Switch the active device to the one with the provided name. Returns zero on success. ~ */
int flipper_select(char *name) {

	struct _lf_device *device = lf_obtain_device(name);
	if(!device) {
		error.raise(E_FLIPPER_NOT_FOUND, ERROR_STRING(E_FLIPPER_NOT_FOUND_S));
		return -1;
	}
	flipper.device = device;
	return 0;
}

/* ~ Attach a Flipper device given an endpoint and name. Returns zero on success. ~ */
int flipper_attach(lf_endpoint endpoint, char *name) {

	/* ~ See if we have already attached a device by the name given. ~ */
	struct _lf_device *device = lf_obtain_device(name);

	/* ~ If we haven't yet attached a device, we must create one. ~ */
	if (!device) {

		/* ~ Allocate memory to contain a new device object. ~ */
		device = malloc(sizeof(struct _lf_device));

		/* ~ Ensure we have been granted the memory. ~*/
		if (!device) {
			error.raise(E_NO_MEM, ERROR_STRING(E_NO_MEM_S));
			return -1;
		}

		/* ~ Associate the name, the identifier, and the default handler. ~ */
		device -> name = name;
		device -> identifier = checksum(name, strlen(name));
		device -> endpoint = endpoint;
		device -> handle = (void *)(-1);

		struct _lf_device *last = flipper.device;
		flipper.device = device;

		/* ~ Select the source. ~ */
		switch (endpoint) {

			case FLIPPER_USB:

				error.clear();

				/* ~ Configure the USB. ~ */
				usb_configure();

				if(error.code != E_OK) {
					flipper.device = last;
					free(device);
					return -1;
				}

				/* ~ Set up the FMR host to accept incoming USB connections. ~ */
				host_configure(&usb);

				if(error.code != E_OK) {
					flipper.device = last;
					free(device);
					return -1;
				}

				/* ~ Set up the FMR device to accept incoming USB connections. ~ */
				device_configure(&usb);

				if(error.code != E_OK) {
					flipper.device = last;
					free(device);
					return -1;
				}

				break;

			case FLIPPER_NETWORK:

				error.clear();

				/* ~ Configure the network. ~ */
				network_configure(name);

				if(error.code != E_OK) {
					flipper.device = last;
					free(device);
					return -1;
				}

				/* ~ Set up the FMR host to accept incoming network connections. ~ */
				host_configure(&network);

				if(error.code != E_OK) {
					flipper.device = last;
					free(device);
					return -1;
				}

				/* ~ Set up the FMR device to accept incoming network connections. ~ */
				device_configure(&network);

				if(error.code != E_OK) {
					flipper.device = last;
					free(device);
					return -1;
				}

				break;

			case FLIPPER_FVM:

				flipper.device = last;
				free(device);
				error.raise(E_UNIMPLEMENTED, ERROR_STRING(E_UNIMPLEMENTED_S));
				return -1;
				break;

			default:
				break;

		}

		if (device -> identifier < 0) {

			/* ~ If we get here, we've failed to attach a valid device. ~ */
			free(device);
			flipper.device = last;

			/* ~ Raise the appropriate error. ~ */
			error.raise(E_HID_NO_DEV, ERROR_STRING("Failed to attach the requested device."));

			return -1;

		}

		device->next = flipper.devices;
		flipper.devices = device;

	}

	return 0;

}

/* ~ Detach a Flipper device given the device name. Returns zero on success. ~ */
int flipper_detach(char *name) {

	// Cursors for current and last devices.
	struct _lf_device *c = flipper.devices;
	struct _lf_device *l = NULL;

	while(c)
	{
		// Check name.
		if(!strcmp(c->name, name))
		{
			break;
		}
		l = c;
		c = c->next;
	}

	// Did we find a device with the provided name?
	if(!c)
	{
		error.raise(E_FLIPPER_NOT_FOUND, ERROR_STRING(E_FLIPPER_NOT_FOUND_S));
		return -1;
	}

	// Detach appropriately.
	switch(c->endpoint)
	{
	case FLIPPER_USB:
		//TODO: actually detach.
		break;
	case FLIPPER_NETWORK:
		//TODO: actually detach.
		break;
	case FLIPPER_FVM:
		//TODO: actually detach.
		break;
	}

	// Detached device wasn't at the head of the list:
	if(l)
	{
		l->next = c->next;
	}
	// Detached device was at the head of the list:
	else
	{
		flipper.devices = c->next;
	}

	free(c);

	return 0;

}
