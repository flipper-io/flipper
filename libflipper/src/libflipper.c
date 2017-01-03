#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/fmr.h>

/* Expose the virtual interface for this driver. */
struct _flipper flipper = {
	flipper_attach,
	flipper_attach_usb,
	flipper_attach_network,
	flipper_attach_endpoint,
	flipper_select,
	flipper_detach,
	flipper_exit,
	E_OK,
	1,
	NULL
};

struct _lf_device *lf_create_device(char *name) {
	/* Allocate memory to contain the record of the device. */
	struct _lf_device *device = (struct _lf_device *)calloc(1, sizeof(struct _lf_device));
	if (!device) {
		error_raise(E_MALLOC, error_message("Failed to allocate the memory required to create a new fmr_device."));
		return NULL;
	}
	if (strlen(name) > sizeof(device -> configuration.name)) {
		error_raise(E_NAME, error_message("The name '%s' is too long. Please choose a name with %lu characters or less.", name, sizeof(device -> configuration.name)));
		goto failure;
	}
	/* Set the device's name. */
	strcpy(device -> configuration.name, name);
	/* Set the device's identifier. */
	device -> configuration.identifier = lf_crc(name, strlen(name));
	return device;
failure:
	free(device);
	return NULL;
}

struct _lf_device *flipper_attach(void) {
	/* Attach a device over USB with the factory default name. */
	return flipper_attach_usb(LF_DEFAULT_NAME);
}

/* Attaches a USB device to the bridge endpoint. */
struct _lf_device *flipper_attach_usb(char *name) {
	/* Create a device with the name provided. */
	struct _lf_device *device = lf_create_device(name);
	if (!device) {
		return NULL;
	}
	/* Set the device's endpoint. */
	device -> endpoint = &lf_bridge_ep;
	/* Configure the device's endpoint. */
	if (device -> endpoint -> configure(device) < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to initialize bridge endpoint for usb device."));
		/* Detach the device in the event of an endpoint configuration failure. */
		flipper_detach(device);
		return NULL;
	}
	/* Set the current device. */
	flipper.device = device;
	return device;
}

struct _lf_device *flipper_attach_network(char *name, char *hostname) {
	struct _lf_device *device = lf_create_device(name);
	if (!device) {
		return NULL;
	}
	/* Set the device's endpoint. */
	device -> endpoint = &lf_network_ep;
	if (device -> endpoint -> configure(device -> endpoint, hostname) < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to initialize endpoint for networked Flipper device."));
		/* Detach the device in the event of an endpoint configuration failure. */
		flipper_detach(device);
		return NULL;
	}
	/* Set the current device. */
	flipper.device = device;
	return device;
}

struct _lf_device *flipper_attach_endpoint(char *name, struct _lf_endpoint *endpoint) {
	struct _lf_device *device = lf_create_device(name);
	if (!device) {
		return NULL;
	}
	/* Set the device's endpoint. */
	device -> endpoint = endpoint;
	/* Set the current device. */
	flipper.device = device;
	return device;
}

int flipper_select(struct _lf_device *device) {
	if (!device) {
		error_raise(E_NULL, error_message("No device provided for selection."));
		return lf_error;
	}
	flipper.device = device;
	return lf_success;
}

int flipper_detach(struct _lf_device *device) {
	if (!device) {
		error_raise(E_NULL, error_message("No device provided for release."));
		return lf_error;
	}
	if (device == flipper.device) {
		flipper.device = NULL;
	}
	if (device -> endpoint) {
		/* If the device has an endpoint, destroy it. */
		device -> endpoint -> destroy(device -> endpoint);
	}
	/* Free the device record structure. */
	free(device);
	return lf_success;
}

int __attribute__((__destructor__)) flipper_exit(void) {
	/* If there is a device attached, free it. */
	if (flipper.device) {
		free(flipper.device);
	}
	return lf_success;
}

int lf_load_configuration(struct _lf_device *device) {
	/* Create a configuration packet. */
	struct _fmr_packet packet = { 0 };
	/* Set the magic number. */
	packet.header.magic = FMR_MAGIC_NUMBER;
	/* Compute the length of the packet. */
	packet.header.length = sizeof(struct _fmr_header);
	/* Make the outgoing packet a configuration packet. */
	packet.header.class = fmr_configuration_class;
	/* Calculate the packet checksum. */
	packet.header.checksum = lf_crc(&packet, packet.header.length);
	/* Send the packet to the target device. */
	int _e = lf_transfer(device, &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	struct _lf_configuration configuration;
	/* Obtain a response packet from the device. */
	_e = device -> endpoint -> pull(device -> endpoint, &configuration, sizeof(struct _lf_configuration));
	if (_e < lf_success) {
		return lf_error;
	}
	/* Obtain the result from the device. */
	struct _fmr_result result;
	/* Obtain the result of the operation. */
	lf_get_result(device, &result);
	if (result.error != E_OK) {
		error_raise(result.error, error_message("Error encountered during configuration."));
		return lf_error;
	}
	/* Compare the device identifiers. */
	if (device -> configuration.identifier != configuration.identifier) {
		error_raise(E_NO_DEVICE, error_message("Identifier mismatch for device '%s'. (0x%04x instead of 0x%04x)", device -> configuration.name, configuration.identifier, device -> configuration.identifier));
		return lf_error;
	}
	/* Copy the returned configuration into the device. */
	memcpy(&(device -> configuration), &configuration, sizeof(struct _lf_configuration));
	return lf_success;
}

/* -- Packet manipulation functions. -- */

int lf_get_result(struct _lf_device *device, struct _fmr_result *result) {
	/* Obtain the response packet from the device. */
	int _e = lf_retrieve(device, result);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Record the observed error code. */
	if (result -> error != E_OK) {
		error_raise(result -> error, error_message("The following error occured on the device '%s':", device -> configuration.name));
	}
	return lf_success;
}

fmr_return lf_invoke(struct _lf_module *module, fmr_function function, struct _fmr_list *parameters) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		error_raise(E_NULL, error_message("No module specified for message runtime invocation."));
		return lf_error;
	}
	/* Obtain the target device from the module. */
	struct _lf_device *device = *(module -> device);
	/* If no device is provided, raise an error. */
	if (!device) {
		error_raise(E_NO_DEVICE, error_message("Failed to invoke on device."));
		return lf_error;
	}
	/* The raw packet into which the invocation information will be loaded .*/
	struct _fmr_packet _packet = { 0 };
	/* A packet cast that exposes the data structures specific to this packet subclass. */
	struct _fmr_invocation_packet *packet = (struct _fmr_invocation_packet *)(&_packet);
	/* Compute the initial length of the packet. */
	_packet.header.length = sizeof(struct _fmr_invocation_packet);
	/* Set the packet class. */
	_packet.header.class = fmr_standard_invocation_class;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_create_call(module -> index, function, parameters, &_packet.header, &packet -> call);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Compute and store the packet checksum. */
	_packet.header.checksum = lf_crc(packet, _packet.header.length);
	/* Send the packet to the target device. */
	_e = lf_transfer(device, (struct _fmr_packet *)(packet));
	if (_e < lf_success) {
		return lf_error;
	}
	struct _fmr_result result;
	/* Obtain the result of the operation. */
	lf_get_result(device, &result);
	/* Return the result of the invocation. */
	return result.value;
}

int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet) {
#ifdef __lf_debug__
	lf_debug_packet(packet, sizeof(struct _fmr_packet));
#endif
	/* Transfer the packet buffer through its registered endpoint. */
	int _e = device -> endpoint -> push(device -> endpoint, packet, sizeof(struct _fmr_packet));
	/* Ensure that the packet was successfully transferred to the device. */
	if (_e < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to transfer packet to device '%s'.", device -> configuration.name));
		return lf_error;
	}
	return lf_success;
}

int lf_retrieve(struct _lf_device *device, struct _fmr_result *result) {
	/* Receive the packet through the device's endpoint. */
	int _e = device -> endpoint -> pull(device -> endpoint, result, sizeof(struct _fmr_result));
	/* Ensure that the packet was successfully obtained from the device. */
	if (_e < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to retrieve packet from the device '%s'.", device -> configuration.name));
		return lf_error;
	}
#ifdef __lf_debug__
	lf_debug_result(result);
#endif
	return lf_success;
}

/* Hacky way to compute the appropriate pointer argument for a device. */
fmr_va fmr_ptr(struct _lf_device *device, void *ptr) {
	if (device -> configuration.attributes & lf_device_32bit) {
		return fmr_int32(ptr);
	} else if (device -> configuration.attributes & lf_device_16bit) {
		return fmr_int16(ptr);
	} else {
		error_raise(E_FMR, error_message("No pointer size specified for the target architecture."));
	}
	return 0;
}

int lf_push(struct _lf_module *module, fmr_function function, void *source, lf_size_t length, struct _fmr_list *parameters) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		error_raise(E_NULL, error_message("No module specified for message runtime push to module '%s'.", module -> name));
		return lf_error;
	} else if (!source) {
		error_raise(E_NULL, error_message("No source provided for message runtime push to module '%s'.", module -> name));
	} else if (!length) {
		return lf_success;
	}
	/* Obtain the target device from the module. */
	struct _lf_device *device = *(module -> device);
	/* If no device is provided, throw an error. */
	if (!device) {
		error_raise(E_NO_DEVICE, error_message("Failed to push to device."));
		return lf_error;
	}
	struct _fmr_packet _packet = { 0 };
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	/* Compute the initial length of the packet. */
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	/* Set the packet class. */
	_packet.header.class = fmr_push_class;
	/* Set the push length. */
	packet -> length = length;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_create_call(module -> index, function, fmr_merge(fmr_args(fmr_ptr(device, source), fmr_infer(length)), parameters), &_packet.header, &packet -> call);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Compute and store the packet checksum. */
	_packet.header.checksum = lf_crc(packet, _packet.header.length);
	/* Send the packet to the target device. */
	_e = lf_transfer(device, &_packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Transfer the data through to the address space of the device. */
	_e = device -> endpoint -> push(device -> endpoint, source, length);
	/* Ensure that the data was successfully transferred to the device. */
	if (_e < lf_success) {
		return lf_error;
	}
	struct _fmr_result result;
	/* Obtain the result of the operation. */
	lf_get_result(device, &result);
	/* Return a pointer to the data. */
	return lf_success;
}

int lf_pull(struct _lf_module *module, fmr_function function, void *destination, lf_size_t length, struct _fmr_list *parameters) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		error_raise(E_NULL, error_message("No module specified for message runtime pull from module '%s'.", module -> name));
		return lf_error;
	} else if (!destination) {
		error_raise(E_NULL, error_message("No destination provided for message runtime pull from module '%s'.", module -> name));
	} else if (!length) {
		return lf_success;
	}
	/* Obtain the target device from the module. */
	struct _lf_device *device = *(module -> device);
	/* If no device is provided, throw an error. */
	if (!device) {
		error_raise(E_NO_DEVICE, error_message("Failed to pull from device."));
		return lf_error;
	}
	struct _fmr_packet _packet = { 0 };
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	/* Compute the initial length of the packet. */
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	/* Set the packet class. */
	_packet.header.class = fmr_pull_class;
	/* Set the pull length. */
	packet -> length = length;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_create_call(module -> index, function, fmr_merge(fmr_args(fmr_ptr(device, destination), fmr_infer(length)), parameters), &_packet.header, &packet -> call);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Compute and store the packet checksum. */
	_packet.header.checksum = lf_crc(packet, _packet.header.length);
	/* Send the packet to the target device. */
	_e = lf_transfer(device, &_packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Obtain the data from the address space of the device. */
	_e = device -> endpoint -> pull(device -> endpoint, destination, length);
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

void lf_debug_call(struct _fmr_call *call) {
	printf("call:\n");
	printf("\t└─ index:\t0x%x\n", call -> index);
	printf("\t└─ function:\t0x%x\n", call -> function);
	printf("\t└─ types:\t0x%x\n", call -> types);
	printf("\t└─ argc:\t0x%x (%d arguments)\n", call -> argc, call -> argc);
	printf("arguments:\n");
	/* Calculate the offset into the packet at which the arguments will be loaded. */
	uint8_t *offset = call -> parameters;
	char *typestrs[] = { "fmr_int8", "fmr_int16", "fmr_int32" };
	fmr_types types = call -> types;
	for (int i = 0; i < call -> argc; i ++) {
		fmr_type type = types & 0x3;
		fmr_arg arg = 0;
		memcpy(&arg, offset, fmr_sizeof(type));
		printf("\t└─ %s:\t0x%x\n", typestrs[type], arg);
		offset += fmr_sizeof(type);
		types >>= 2;
	}
	printf("\n");
}

void lf_debug_packet(struct _fmr_packet *packet, size_t length) {
	if (packet -> header.magic == FMR_MAGIC_NUMBER) {
		printf("header:\n");
		printf("\t└─ magic:\t0x%x\n", packet -> header.magic);
		printf("\t└─ checksum:\t0x%x\n", packet -> header.checksum);
		printf("\t└─ length:\t%d bytes (%.02f%%)\n", packet -> header.length, (float) packet -> header.length/sizeof(struct _fmr_packet)*100);
        char *classstrs[] = { "configuration", "std_call", "user_call", "push", "pull", "event" };
        printf("\t└─ class:\t%s\n", classstrs[packet -> header.class]);
		struct _fmr_invocation_packet *invocation = (struct _fmr_invocation_packet *)(packet);
		struct _fmr_push_pull_packet *pushpull = (struct _fmr_push_pull_packet *)(packet);
		switch (packet -> header.class) {
			case fmr_configuration_class:
			break;
			case fmr_standard_invocation_class:
				lf_debug_call(&invocation -> call);
			break;
			case fmr_user_invocation_class:
			break;
			case fmr_push_class:
			case fmr_pull_class:
				printf("length:\n");
				printf("\t└─ length:\t0x%x\n", pushpull -> length);
				lf_debug_call(&pushpull -> call);
			break;
			default:
				printf("Invalid packet class.\n");
			break;
		}
		for (int i = 1; i <= length; i ++) {
			printf("0x%02x ", ((uint8_t *)packet)[i - 1]);
			if (i % 8 == 0 && i < length - 1) printf("\n");
		}
	} else {
		printf("Invalid magic number (0x%02x).\n", packet -> header.magic);
	}
	printf("\n\n-----------\n\n");
}

void lf_debug_result(struct _fmr_result *result) {
	printf("response:\n");
	printf("\t└─ value:\t0x%x\n", result -> value);
	printf("\t└─ error:\t0x%x\n", result -> error);
	printf("\n-----------\n\n");
}
