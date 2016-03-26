#define __private_include__
#include <flipper/flipper.h>
#include <fmr/fmr.h>
#include <platform.h>
#include <usb/usb.h>
#include <dlfcn.h>
#include <network.h>

struct _flipper flipper = {
	flipper_attach
};

/* ~ Obtain a device and endpoint for a given device name. ~ */
struct _lf_device *lf_obtain_device(char *name) {

	struct _lf_device *current = flipper.devices;

	while (current) {

		/* ~ See if we have a match. ~ */
		if (!strcmp(current -> name, name)) return current;

		/* ~ If we don't, advance to the next attached device. ~ */
		current = current -> next;

	}

	return NULL;
	
}

/* ~ Attach a Flipper device given an endpoint and name. ~ */
void flipper_attach(lf_endpoint endpoint, char *name) {

	/* ~ Cache a copy of the last device we were attached to. ~ */
	struct _lf_device *last = flipper.device;

	/* ~ See if we have already attached a device by the name given. ~ */
	struct _lf_device *device = lf_obtain_device(name);

	/* ~ If we haven't yet attached a device, we must create one. ~ */
	if (!device) {

		/* ~ Allocate memory to contain a new device object. ~ */
		device = malloc(sizeof(struct _lf_device));

		/* ~ Ensure we have been granted the memory. ~*/
		if (!device) error.raise(E_NO_MEM, ERROR_STRING(E_NO_MEM_S));

		/* ~ Associate the name, the identifier, and the default handler. ~ */
		device -> name = name;
		device -> identifier = checksum(name, strlen(name));
		device -> handle = (void *)(-1);

		/* ~ Set the global device pointer equal to the newly allocated device. ~ */
		flipper.device = device;

		/* ~ Select the source. ~ */
		switch (endpoint) {

			case FLIPPER_USB:

				/* ~ Configure the USB. ~ */
				usb_configure();

				/* ~ Set up the FMR host to accept incoming USB connections. ~ */
				host_configure(&usb);

				/* ~ Set up the FMR device to accept incoming USB connections. ~ */
				device_configure(&usb);

				break;

			case FLIPPER_NETWORK:

				/* ~ Configure the network. ~ */
				network_configure(name);

				/* ~ Set up the FMR host to accept incoming network connections. ~ */
				host_configure(&network);

				/* ~ Set up the FMR device to accept incoming network connections. ~ */
				device_configure(&network);

				break;

			case FLIPPER_FVM:
				break;

			default:
				break;

		}

		if (flipper.device -> identifier < 0) {

			/* ~ Raise the appropriate error. ~ */
			error.raise(E_HID_NO_DEV, ERROR_STRING("Failed to attach the requested device."));

			/* ~ If we get here, we've failed to attach a valid device. ~ */
			free(device);

			/* ~ Restore the last device that we were attached to. ~ */
			flipper.device = last;

			return;

		}

		/* ~ Store the new device in the linked list of attached devices. ~ */
		struct _lf_device *current = flipper.devices;

		/* ~ If the device list is not empty, walk it until we can append the new device. ~ */
		if (current) {
			while (current -> next) current = current -> next;
			current -> next = device;
		}

		else {
			/* ~ If we haven't yet attached a device, make it the head of the device list. ~ */
			flipper.devices = device;
		}

	}

}