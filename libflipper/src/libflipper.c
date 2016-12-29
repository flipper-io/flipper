#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>
#include <platforms/posix.h>

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

struct _lf_device *lf_attach(char *name, struct _lf_endpoint *endpoint) {
	/* Ensure a valid endpoint was provided. */
	if (!endpoint) {
		error_raise(E_ENDPOINT, error_message("No endpoint provided for the device '%s'.", name));
		goto  failure;
	}
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
	/* Set the device's endpoint. */
	device -> endpoint = endpoint;
	// /* Create the device's identifier. */
	// lf_crc_t _identifier = lf_crc(name, strlen(name));
	// /* Broadcast a packet to the device over its endpoint to verify the identifier. */
	// int _e = lf_load_configuration(device);
	// if (_e < lf_success) {
	// 	error_raise(E_CONFIGURATION, error_message("Failed to obtain configuration for device '%s'.", name));
	// 	goto failure;
	// }
	// /* Compare the device identifiers. */
	// if (device -> configuration.identifier != _identifier) {
	// 	error_raise(E_NO_DEVICE, error_message("Identifier mismatch for device '%s'. (0x%04x instead of 0x%04x)", name, device -> configuration.identifier, _identifier));
	// 	goto failure;
	// }
	/* Set the current device. */
	flipper.device = device;
	return device;
failure:
	free(device);
	return NULL;
}

struct _lf_device *flipper_attach(void) {
	/* Attach a device over USB with the factory default name. */
	return flipper_attach_usb("flipper");
}

/* Attaches a USB device to the bridge endpoint. */
struct _lf_device *flipper_attach_usb(char *name) {
	struct _lf_endpoint *_ep = &lf_bridge_ep;
	if (_ep -> configure(_ep) < lf_success) {
		return NULL;
	}
	return lf_attach(name, _ep);
}

struct _lf_device *flipper_attach_network(char *name, char *hostname) {
	struct _lf_endpoint *_ep = &lf_network_ep;
	if (_ep -> configure(_ep, hostname) < lf_success) {
		return NULL;
	}
	return lf_attach(name, _ep);
}

struct _lf_device *flipper_attach_endpoint(char *name, struct _lf_endpoint *endpoint) {
	if (endpoint -> configure(endpoint) < lf_success) {
		return NULL;
	}
	return lf_attach(name, endpoint);
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
	/* Create a configuration packet. */
	struct _fmr_configuration_packet packet = { 0 };
	/* Set the magic number. */
	packet.header.magic = FMR_MAGIC_NUMBER;
	/* Compute the length of the packet. */
	packet.header.length = sizeof(struct _fmr_header);
	/* Make the outgoing packet a configuration packet. */
	packet.header.class = fmr_configuration_class;
	/* Calculate the packet checksum. */
	packet.header.checksum = lf_crc(&packet, packet.header.length);
	/* Send the packet to the target device. */
	int _e = lf_transfer(device, (struct _fmr_packet *)(&packet));
	if (_e < lf_success) {
		return lf_error;
	}
	/* Obtain a response packet from the device. */
	_e = device -> endpoint -> pull(device -> endpoint, &(device -> configuration), sizeof(struct _lf_configuration));
	if (_e < lf_success) {
		return lf_error;
	}
	/* Obtain the result from the device. */
	struct _fmr_result result;
	/* Obtain the result of the operation. */
	lf_get_result(device, &result);
	if (result.error == E_OK) {
		return lf_success;
	}
	return lf_error;
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
	/* Ensure that the device pointer is valid. */
	struct _lf_device *device = *(module -> device);
	/* If no device is provided, raise an error. */
	if (!device) {
		error_raise(E_NO_DEVICE, error_message("Failed to invoke on device."));
		return lf_error;
	}
	struct _fmr_invocation_packet packet = { 0 };
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_generate(module -> index, function, parameters, &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Send the packet to the target device. */
	_e = lf_transfer(device, (struct _fmr_packet *)(&packet));
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

int lf_retrieve(struct _lf_device *device, struct _fmr_result *response) {
	/* Receive the packet through the device's endpoint. */
	int _e = device -> endpoint -> pull(device -> endpoint, response, sizeof(struct _fmr_result));
	/* Ensure that the packet was successfully obtained from the device. */
	if (_e < lf_success) {
		error_raise(E_ENDPOINT, error_message("Failed to retrieve packet from the device '%s'.", device -> configuration.name));
		return lf_error;
	}
	return lf_success;
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
	/* Ensure that the device pointer is valid. */
	struct _lf_device *device = *(module -> device);
	/* If no device is provided, throw an error. */
	if (!device) {
		error_raise(E_NO_DEVICE, error_message("Failed to push to device."));
		return lf_error;
	}
	struct _fmr_invocation_packet packet = { 0 };
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_generate(_fmr_id, _fmr_push, fmr_merge(fmr_args(fmr_int16(module -> index), fmr_int8(function), fmr_int32(length)), parameters), &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Send the packet to the target device. */
	_e = lf_transfer(device, (struct _fmr_packet *)(&packet));
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
	struct _lf_device *device = *(module -> device);
	/* If no device is provided, throw an error. */
	if (!device) {
		error_raise(E_NO_DEVICE, error_message("Failed to pull from device."));
		return lf_error;
	}
	struct _fmr_invocation_packet packet = { 0 };
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_generate(_fmr_id, _fmr_pull, fmr_merge(fmr_args(fmr_int16(module -> index), fmr_int8(function), fmr_int32(length)), parameters), &packet);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Send the packet to the target device. */
	_e = lf_transfer(device, (struct _fmr_packet *)(&packet));
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

int lf_bind(struct _lf_module *module) {
	/* Calculate the module's identifier. */
	module -> identifier = lf_crc(module -> name, strlen(module -> name));
	/* Bind the module to a slot on the device. */
	module -> index = fld_bind(module -> identifier);
	if (!module -> index) {
		error_raise(E_MODULE, error_message("No slot given."));
		return lf_error;
	}
	return lf_success;
}

/* Debugging functions for displaying the contents of various FMR related data structures. */

void lf_debug_packet(struct _fmr_packet *packet, size_t length) {
	if (packet -> header.magic == FMR_MAGIC_NUMBER) {
		printf("header:\n");
		printf("\t└─ magic:\t0x%x\n", packet -> header.magic);
		printf("\t└─ checksum:\t0x%x\n", packet -> header.checksum);
		printf("\t└─ length:\t%d bytes (%.02f%%)\n", packet -> header.length, (float) packet -> header.length/sizeof(struct _fmr_packet)*100);
        char *classstrs[] = { "configuration", "std_call", "user_call", "event" };
        printf("\t└─ class:\t%s\n", classstrs[packet -> header.class]);
		/* Print different information depending on the packet class. */
		if (packet -> header.class == fmr_configuration_class) {

        } else if (packet -> header.class == fmr_standard_invocation_class) {
			struct _fmr_invocation_packet *invocation = (struct _fmr_invocation_packet *)(packet);
			printf("target:\n");
			printf("\t└─ module:\t0x%x\n", invocation -> call.index);
			printf("\t└─ function:\t0x%x\n", invocation -> call.function);
			printf("\t└─ argc:\t0x%x (%d arguments)\n", invocation -> call.argc, invocation -> call.argc);
			printf("arguments:\n");
			/* Calculate the number of bytes needed to encode the widths of the types. */
			uint8_t encode_length = lf_ceiling((invocation -> call.argc * 2), 8);
			/* Calculate the offset into the packet at which the arguments will be loaded. */
			uint8_t *offset = invocation -> parameters + encode_length;
			/* Create a buffer for encoding argument types. */
			uint32_t types = 0;
			memcpy(&types, invocation -> parameters, encode_length);
			char *typestrs[] = { "fmr_int8", "fmr_int16", "fmr_int32" };
			for (int i = 0; i < invocation -> call.argc; i ++) {
				fmr_type type = types & 0x3;
				fmr_arg arg = 0;
				memcpy(&arg, offset, fmr_sizeof(type));
				printf("\t└─ %s:\t0x%x\n", typestrs[type], arg);
				offset += fmr_sizeof(type);
				types >>= 2;
			}
			printf("\n");
		} else {
			printf("Invalid packet.\n");
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
