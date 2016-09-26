#define __private_include__
#include <flipper/flipper.h>
#include <flipper/error.h>
#include <platform/posix.h>

struct _lf_device *lf_attach(char *name) {
	/* Allocate memory to contain the record of the device. */
	struct _lf_device *device = (struct _lf_device *)calloc(1, sizeof(struct _lf_device));
	if (!device) {
		error_raise(E_MALLOC, error_message("Failed to allocate the memory required to create a new fmr_device."));
		return NULL;
	}
	/* Set the device's name. */
	device -> name = name;
	/* Set the device's identifier. */
	device -> identifier = lf_checksum(name, strlen(name));
	/* Cause the device to generate error related side effects on the host. */
	device -> errors_generate_side_effects = true;
	/* Obtain the head of the linked list of attached devices. */
	struct _lf_device *last, *head = flipper.attached;
	last = head;
	/* Check if any devices have been attached. */
	if (!head) {
		/* If we don't yet have a head, make this device the head of the linked list. */
		flipper.attached = device;
	}
	else {
		/* Walk the list of attached devices until the desired device is found. */
		while (head) {
			/* Compare the name of the attached device with the name of the new device. */
			if (!strcmp(head -> name, device -> name)) {
				/* If a device with the given name has already been attached, raise an error. */
				error_raise(E_ALREADY_ATTACHED, error_message("Could not attach a device named '%s'. A device with that name has already been attached.", device -> name));
				goto failure;
			}
			/* Advance to the next attached device. */
			last = head;
			head = head -> next;
		}
		/* Stage the device into the list of attached devices. */
		last -> next = device;
	}
	/* Set the current device. */
	flipper.device = device;
	/* Return with success. */
	return device;
failure:
	free(device);
	return NULL;
}

int flipper_attach(void) {
	/* Attach a device over USB with the factory default name. */
	return flipper_attach_usb("flipper");
}

int flipper_attach_usb(char *name) {
	struct _lf_device *device = lf_attach(name);
	if (!device) {
		return lf_error;
	}
	const struct _lf_endpoint *endpoint = &libusb;
	/* Set the device's endpoint. */
	device -> endpoint = endpoint;
	/* Configure the device's endpoint. */
	int8_t _e = endpoint -> configure(endpoint);
	if (_e < lf_success) {
		error_raise(E_NO_DEVICE, error_message("Failed to connect to Flipper device."));
	}
	/* Broadcast a packet to the device over the default endpoint to verify the identifier. */
	//!MISSING!
	/* Ask the device for its attributes. */
	//!MISSING!
	device -> attributes = lf_device_32bit | lf_device_little_endian;
	return lf_success;
}

int flipper_attach_network(char *name, char *hostname) {
	struct _lf_device *device = lf_attach(name);
	if (!device) {
		return lf_error;
	}
	const struct _lf_endpoint *endpoint = &lf_network_ep;
	/* Set the device's endpoint. */
	device -> endpoint = endpoint;
	/* Configure the device's endpoint. */
	endpoint -> configure(endpoint, hostname);
	/* Broadcast a packet to the device over the default endpoint to verify the identifier. */
	//!MISSING!
	/* Ask the device for its attributes. */
	//!MISSING!
	device -> attributes = lf_device_32bit | lf_device_little_endian;
	return lf_success;
}

int flipper_attach_endpoint(char *name, const struct _lf_endpoint *endpoint) {
	struct _lf_device *device = lf_attach(name);
	if (!device) {
		return lf_error;
	}
	/* Set the device's endpoint. */
	device -> endpoint = endpoint;
	/* Configure the device's endpoint. */
	endpoint -> configure(endpoint);
	/* Broadcast a packet to the device over the default endpoint to verify the identifier. */
	//!MISSING!
	/* Ask the device for its attributes. */
	//!MISSING!
	device -> attributes = lf_device_32bit | lf_device_little_endian;
	return lf_error;
}

int flipper_select(char *name) {
	/* Obtain the head of the linked list of attached devices. */
	struct _lf_device *head = flipper.attached;
	/* Walk the list of attached devices until the desired device is found. */
	while (head) {
		/* Compare the name of the attached device with the name provided. */
		if (!strcmp(head -> name, name)) {
			/* If we have a match, set the current device. */
			flipper.device = head;
			return lf_success;
		}
		/* Advance to the next attached device. */
		head = head -> next;
	}
	/* If we get here, no device with the name provided could be found. */
	error_raise(E_NOT_ATTACHED, error_message("Failed to select device named '%s'.", name));
	return lf_error;
}

int flipper_release(struct _lf_device *device) {
	/* Destroy the endpoint. */
	device -> endpoint -> destroy();
	/* If the device's endpoint previously allocated memory to contain a record, release it. */
	if (device -> endpoint -> record) {
		free(device -> endpoint -> record);
	}
	/* If the device we are detaching is the actively selected device, nullify it. */
	if (device == flipper.attached) {
		flipper.attached = device -> next;
	}
	if (device == flipper.device) {
		flipper.device = NULL;
	}
	/* Free the device record structure. */
	free(device);
	return lf_error;
}

int flipper_detach(char *name) {
	/* Obtain the head of the linked list of attached devices. */
	struct _lf_device *device = flipper.attached;
	/* Walk the list of attached devices until the desired device is found. */
	while (device) {
		/* Compare the name of the attached device with the name provided. */
		if (!strcmp(device -> name, name)) {
			/* If we have isolated the device to detach, release it. */
			return flipper_release(device);
		}
		/* Advance to the next device. */
		device = device -> next;
	}
	/* If we get here, no device with the name provided could be found. */
	error_raise(E_NOT_ATTACHED, error_message("Failed to detach a device named '%s'.", name));
	return lf_error;
}

int __attribute__((__destructor__)) flipper_exit(void) {
	/* Walk the attached list and detach all attached devices. */
	struct _lf_device *head = flipper.attached;
	/* Walk the list of attached devices until the desired device is found. */
	while (head) {
		struct _lf_device *next = head -> next;
		/* Release the device. */
		flipper_release(head);
		/* Advance to the next attached device. */
		head = next;
	}
	flipper.attached = NULL;
	return lf_success;
}

fmr_type lf_word_size(struct _lf_device *device) {
	/* If no device is selected, raise an error. */
	if (!device) {
		error_raise(E_NO_DEVICE, error_message("Failed to determine word size."));
		return 0;
	}
	/* Extract and return the device's word size from its attributes. */
	return (device -> attributes >> 1) & 0x7;
}

/* -- Packet manipulation functions. -- */

int lf_invoke(struct _fmr_module *module, fmr_function function, struct _fmr_list *args) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		error_raise(E_NULL, error_message("No module specified for message runtime invocation."));
		return lf_error;
	}
	/* Ensure that the argument list is valid. */
	if (!args) {
		error_raise(E_NULL, error_message("No arguments specified for message runtime invocation."));
		return lf_error;
	}
	/* Ensure that the device pointer is valid. */
	struct _lf_device *device = module -> device;
	if (!device) {
		error_raise(E_NULL, error_message("No device specified for message runtime invocation."));
		return lf_error;
	}
	struct _fmr_packet packet;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_generate(module, function, args, &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Send the packet to the target device. */
	_e = lf_transfer_packet(device, &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Obtain the response packet from the device. */
	_e = lf_retrieve_packet(device, &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Parse the response packet from the device. */
	struct _fmr_result result;
	memcpy(&result, &packet, sizeof(struct _fmr_result));
	/* Synchronize with the device's error state. */
	device -> error = result.error;
	/* If the device encountered an error, raise it. */
	if (device -> error != E_OK) {
		error_raise(E_LAST, error_message("The device '%s' encountered the following error:", device -> name));
	}
	/* Return the result of the invocation. */
	return result.value;
}

int lf_transfer_packet(struct _lf_device *device, struct _fmr_packet *packet) {
	/* Transfer the packet buffer through its registered endpoint. */
	int _e = device -> endpoint -> push(packet, sizeof(struct _fmr_packet));
	/* Ensure that the packet was successfully transferred to the device. */
	if (_e < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to transfer packet to device '%s'.", device -> name));
		return lf_error;
	}
	return lf_success;
}

int lf_retrieve_packet(struct _lf_device *device, struct _fmr_packet *packet) {
	/* Receive the packet through the device's endpoint. */
	int _e = device -> endpoint -> pull(packet, sizeof(struct _fmr_packet));
	/* Ensure that the packet was successfully obtained from the device. */
	if (_e < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to retrieve packet from the device '%s'.", device -> name));
		return lf_error;
	}
	return lf_success;
}

/* Debugging functions for displaying the contents of various FMR related data structures. */

void lf_debug_packet(struct _fmr_packet *packet) {
	printf("- Message runtime packet deconstruction. -\n\n");
	printf("header:\n");
	printf("\t└─ magic:\t0x%x\n", packet -> header.magic);
	printf("\t└─ checksum:\t0x%x\n", packet -> header.checksum);
	printf("\t└─ length:\t%d bytes\n", packet -> header.length);
	printf("target:\n");
	printf("\t└─ module:\t0x%x\n", packet -> target.module);
	printf("\t└─ function:\t0x%x\n", packet -> target.function);
	printf("\t└─ argc:\t0x%x (%d arguments)\n", packet -> target.argc, packet -> target.argc);
	printf("arguments:\n");
	/* Calculate the number of bytes needed to encode the widths of the types. */
	uint8_t encode_length = lf_ceiling((packet -> target.argc * 2), 8);
	/* Calculate the offset into the packet at which the arguments will be loaded. */
	uint8_t *offset = packet -> body + encode_length;
	/* Create a buffer for encoding argument types. */
	uint32_t types = 0;
	memcpy(&types, packet -> body, encode_length);
	char *typestrs[] = { "fmr_int8", "fmr_int16", "fmr_int32" };
	for (int i = 0; i < packet -> target.argc; i ++) {
		fmr_type type = types & 0x3;
		fmr_arg arg = 0;
		memcpy(&arg, offset, fmr_sizeof(type));
		printf("\t└─ %s:\t0x%x\n", typestrs[type], arg);
		offset += fmr_sizeof(type);
		types >>= 2;
	}
	printf("\nRaw packet data:\n\n");
	for (int i = 1; i <= sizeof(struct _fmr_packet); i ++) {
		printf("0x%02x ", ((uint8_t *)packet)[i - 1]);
		if (i % 8 == 0) printf("\n");
	}
	printf("\n");
}
