#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>
#include <platform/posix.h>

int lf_attach(char *name, struct _lf_endpoint *endpoint) {
	/* Allocate memory to contain the record of the device. */
	struct _lf_device *device = (struct _lf_device *)calloc(1, sizeof(struct _lf_device));
	if (!device) {
		error_raise(E_MALLOC, error_message("Failed to allocate the memory required to create a new fmr_device."));
		return lf_error;
	}
	if (strlen(name) > sizeof(device -> configuration.name)) {
		error_raise(E_NAME, error_message("The name '%s' is too long. Please choose a name with %i characters or less.", name, sizeof(device -> configuration.name)));
		goto failure;
	}
	/* Set the device's name. */
	strcpy(device -> configuration.name, name);
	/* Set the device's identifier. */
	device -> configuration.identifier = lf_checksum(name, strlen(name));
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
			if (!strcmp(head -> configuration.name, device -> configuration.name)) {
				/* If a device with the given name has already been attached, raise an error. */
				error_raise(E_ALREADY_ATTACHED, error_message("Could not attach a device named '%s'. A device with that name has already been attached.", device -> configuration.name));
				goto failure;
			}
			/* Advance to the next attached device. */
			last = head;
			head = head -> next;
		}
		/* Stage the device into the list of attached devices. */
		last -> next = device;
	}
	/* Set the device's endpoint. */
	device -> endpoint = endpoint;
	if (!endpoint) {
		error_raise(E_ENDPOINT, error_message("No endpoint provided for the device '%s'.", name));
		goto  failure;
	}
	/* Set the current device. */
	flipper.device = device;
	/* Save the test identifier. */
	lf_id_t _identifier = device -> configuration.identifier;
	/* Broadcast a packet to the device over the default endpoint to verify the identifier. */
	int _e = lf_load_configuration(device);
	if (_e < lf_success) {
		error_raise(E_CONFIGURATION, error_message("Failed to obtain configuration for device '%s'.", name));
		goto failure;
	}
	/* Compare the device identifiers. */
	if (device -> configuration.identifier != _identifier) {
		error_raise(E_NO_DEVICE, error_message("Identifier mismatch for device '%s'. (0x%04x instead of 0x%04x)", name, device -> configuration.identifier, _identifier));
		goto failure;
	}
	/* Return with success. */
	return lf_success;
failure:
	free(device);
	return lf_error;
}

int flipper_attach(void) {
	/* Attach a device over USB with the factory default name. */
	return flipper_attach_usb("flipper");
}

int flipper_attach_usb(char *name) {
	struct _lf_endpoint *_ep = &lf_usb_ep;
	_ep -> configure(_ep);
	return lf_attach(name, _ep);
}

int flipper_attach_network(char *name, char *hostname) {
	struct _lf_endpoint *_ep = &lf_network_ep;
	_ep -> configure(_ep, hostname);
	return lf_attach(name, _ep);
}

int flipper_attach_endpoint(char *name, struct _lf_endpoint *endpoint) {
	endpoint -> configure(endpoint);
	return lf_attach(name, endpoint);
}

int flipper_select(char *name) {
	/* Obtain the head of the linked list of attached devices. */
	struct _lf_device *head = flipper.attached;
	/* Walk the list of attached devices until the desired device is found. */
	while (head) {
		/* Compare the name of the attached device with the name provided. */
		if (!strcmp(head -> configuration.name, name)) {
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
	if (!device) {
		error_raise(E_NULL, error_message("No device provided for release."));
		return lf_error;
	}
	/* If the device has an endpoint, deallocate it. */
	if (device -> endpoint) {
		device -> endpoint -> destroy();
		/* If the device's endpoint previously allocated memory to contain a record, release it. */
		if (device -> endpoint -> record) {
			free(device -> endpoint -> record);
		}
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
		if (!strcmp(device -> configuration.name, name)) {
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
	return (device -> configuration.attributes >> 1) & 0x7;
}

int lf_load_configuration(struct _lf_device *device) {
	struct _fmr_packet packet;
	/* Generate the a null procedure call in the outgoing packet. */
	int _e = fmr_generate(0, 0, fmr_build(0), &packet);
	/* Set the configuration bit in the target attributes. */
	packet.target.attributes |= LF_CONFIGURATION;
	/* Send the packet to the target device. */
	_e = lf_transfer(device, &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Obtain a response packet from the device. */
	_e = device -> endpoint -> pull(&(device -> configuration), sizeof(struct _lf_configuration));
	if (_e < lf_success) {
		return lf_error;
	}
	return lf_success;
}

struct _lf_device *lf_device(void) {
	if (!flipper.device) {
		error_raise(E_NO_DEVICE, error_message("No device selected. Please attach a device."));
		return NULL;
	}
	return flipper.device;
}

/* -- Packet manipulation functions. -- */

int lf_get_result(struct _lf_device *device, struct _fmr_result *result) {
	/* Obtain the response packet from the device. */
	int _e = lf_retrieve(device, result);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Synchronize with the device's error state. */
	device -> error = result -> error;
	/* If the device encountered an error, raise it. */
	if (device -> error != E_OK) {
		error_raise(E_LAST, error_message("The following error occured on the device '%s':", device -> configuration.name));
	}
	return lf_success;
}

int lf_invoke(struct _fmr_module *module, fmr_function function, struct _fmr_list *args) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		error_raise(E_NULL, error_message("No module specified for message runtime invocation."));
		return lf_error;
	}
	/* Ensure that the device pointer is valid. */
	struct _lf_device *device = module -> device;
	/* If no device is provided, send the request to the actively selected device. */
	if (!device) {
		device = lf_device();
	}
	struct _fmr_packet packet;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_generate(module -> identifier, function, args, &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Send the packet to the target device. */
	_e = lf_transfer(device, &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Obtain the result of the operation. */
	struct _fmr_result result;
	lf_get_result(device, &result);
	/* Return the result of the invocation. */
	return result.value;
}

int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet) {
	/* Transfer the packet buffer through its registered endpoint. */
	int _e = device -> endpoint -> push(packet, sizeof(struct _fmr_packet));
	/* Ensure that the packet was successfully transferred to the device. */
	if (_e < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to transfer packet to device '%s'.", device -> configuration.name));
		return lf_error;
	}
	return lf_success;
}

int lf_retrieve(struct _lf_device *device, struct _fmr_result *response) {
	/* Receive the packet through the device's endpoint. */
	int _e = device -> endpoint -> pull(response, sizeof(struct _fmr_result));
	/* Ensure that the packet was successfully obtained from the device. */
	if (_e < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to retrieve packet from the device '%s'.", device -> configuration.name));
		return lf_error;
	}
	return lf_success;
}

void *lf_push(struct _fmr_module *module, fmr_function function, void *source, lf_size_t length) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		error_raise(E_NULL, error_message("No module specified for message runtime push to module '%s'.", module -> name));
		return NULL;
	}
	if (!source) {
		error_raise(E_NULL, error_message("No source provided for message runtime push to module '%s'.", module -> name));
	}
	if (!length) {
		return lf_success;
	}
	/* Ensure that the device pointer is valid. */
	struct _lf_device *device = module -> device;
	/* If no device is provided, send the request to the actively selected device. */
	if (!device) {
		device = lf_device();
	}
	/* Call the device's fmr_push_handler with metadata about the transfer. */
	struct _fmr_packet packet;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_generate(_fmr_id, _fmr_push, fmr_args(fmr_int16(module -> identifier), fmr_int8(function), fmr_int32(length)), &packet);
	if (_e < lf_success) {
		return NULL;
	}
	/* Send the packet to the target device. */
	_e = lf_transfer(device, &packet);
	if (_e < lf_success) {
		return NULL;
	}

	/* Send the data to the device. */
	_e = device -> endpoint -> push(source, length);
	/* Ensure that the data was successfully transferred to the device. */
	if (_e < lf_success) {
		return NULL;
	}

	struct _fmr_result result;
	/* Obtain the result of the operation. */
	lf_get_result(device, &result);
	/* Return a pointer to the data. */
	return (void *)((uintptr_t)result.value);
}

int lf_pull(struct _fmr_module *module, fmr_function function, void *destination, lf_size_t length) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		error_raise(E_NULL, error_message("No module specified for message runtime pull from module '%s'.", module -> name));
		return lf_error;
	}
	if (!destination) {
		error_raise(E_NULL, error_message("No destination provided for message runtime pull from module '%s'.", module -> name));
	}
	if (!length) {
		return lf_success;
	}
	/* Ensure that the device pointer is valid. */
	struct _lf_device *device = module -> device;
	/* If no device is provided, send the request to the actively selected device. */
	if (!device) {
		device = lf_device();
	}
	/* Call the device's fmr_push_handler with metadata about the transfer. */
	struct _fmr_packet packet;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_generate(_fmr_id, _fmr_pull, fmr_args(fmr_int16(module -> identifier), fmr_int8(function), fmr_int32(length)), &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Send the packet to the target device. */
	_e = lf_transfer(device, &packet);
	if (_e < lf_success) {
		return lf_error;
	}

	/* Send the data to the device. */
	_e = device -> endpoint -> pull(destination, length);
	/* Ensure that the data was successfully transferred to the device. */
	if (_e < lf_success) {
		return lf_error;
	}

	struct _fmr_result result;
	/* Obtain the result of the operation. */
	lf_get_result(device, &result);
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
