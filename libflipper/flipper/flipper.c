#define __private_include__
#include <flipper/flipper.h>
#include <flipper/fmr.h>
#include <flipper/usb.h>
#include <flipper/platform/network.h>
#include <flipper/platform/platform.h>

const struct _flipper flipper = {
	flipper_attach,
	flipper_detach,
	flipper_select,
};

struct _lf_device *flipper_device = NULL;
struct _lf_device *flipper_devices = NULL;

/* ~ Obtain a device and endpoint for a given device name. ~ */
struct _lf_device *lf_obtain_device(char *name) {

	struct _lf_device *current = flipper_devices;

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
	flipper_device = device;
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

		struct _lf_device *last = flipper_device;
		flipper_device = device;

		error.clear();

		/* ~ Select the source. ~ */
		switch (endpoint) {

			case FLIPPER_USB:

				/* ~ Set up the FMR host to accept incoming USB connections. ~ */
				host_configure(&usb);

				/* ~ Set up the FMR device to accept incoming USB connections. ~ */
				device_configure(&usb);

				/* ~ Configure the USB. ~ */
				usb_configure();

				if(error.get() != E_OK) {
					flipper_device = last;
					free(device);
					return -1;
				}

				break;

			case FLIPPER_NETWORK:

				/* ~ Set up the FMR host to accept incoming network connections. ~ */
				host_configure(&network);

				/* ~ Set up the FMR device to accept incoming network connections. ~ */
				device_configure(&network);

				/* ~ Configure the network. ~ */
				network_configure(name);

				if(error.get() != E_OK) {
					flipper_device = last;
					free(device);
					return -1;
				}

				break;

			case FLIPPER_FVM:

				flipper_device = last;
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
			flipper_device = last;

			/* ~ Raise the appropriate error. ~ */
			error.raise(E_HID_NO_DEV, ERROR_STRING("Failed to attach the requested device."));

			return -1;

		}

		device->next = flipper_devices;
		flipper_devices = device;

	}

	return 0;

}

/* ~ Detach a Flipper device given the device name. Returns zero on success. ~ */
int flipper_detach(char *name) {

	// Cursors for current and last devices.
	struct _lf_device *c = flipper_devices;
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
		flipper_devices = c->next;
	}

	free(c);

	return 0;

}
