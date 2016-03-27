#define __private_include__
#include <flipper/flipper/flipper.h>
#include <flipper/fmr/fmr.h>
#include <flipper/usb/usb.h>

#include <network.h>
#include <platform.h>

struct _flipper flipper = {
	flipper_attach,
	flipper_select,
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
		device -> handle = (void *)(-1);

		/* ~ Select the source. ~ */
		switch (endpoint) {

			case FLIPPER_USB:

				error.clear();

				/* ~ Configure the USB. ~ */
				usb_configure();

				if(error.code != E_OK) {
					return -1;
				}

				/* ~ Set up the FMR host to accept incoming USB connections. ~ */
				host_configure(&usb);

				if(error.code != E_OK) {
					return -1;
				}

				/* ~ Set up the FMR device to accept incoming USB connections. ~ */
				device_configure(&usb);

				if(error.code != E_OK) {
					return -1;
				}

				break;

			case FLIPPER_NETWORK:

				error.clear();

				/* ~ Configure the network. ~ */
				network_configure(name);

				if(error.code != E_OK) {
					return -1;
				}

				/* ~ Set up the FMR host to accept incoming network connections. ~ */
				host_configure(&network);

				if(error.code != E_OK) {
					return -1;
				}

				/* ~ Set up the FMR device to accept incoming network connections. ~ */
				device_configure(&network);

				if(error.code != E_OK) {
					return -1;
				}

				break;

			case FLIPPER_FVM:

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

			/* ~ Raise the appropriate error. ~ */
			error.raise(E_HID_NO_DEV, ERROR_STRING("Failed to attach the requested device."));

			return -1;

		}

		/* ~ Store the new device in the linked list of attached devices. ~ */
		struct _lf_device *current = flipper.devices;

		device->next = flipper.devices;
		flipper.devices = device;
		flipper.device = device;

	}

	return 0;

}
