#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/fmr.h>

/* Include the Carbon board file. */
#include <flipper/carbon.h>

lf_device_list lf_attached_devices;
struct _lf_device *lf_current_device;
lf_event_list lf_registered_events;

/* Expose the virtual interface for this driver. */
struct _flipper flipper = {
	flipper_attach,
	flipper_attach_usb,
	flipper_attach_network,
	flipper_select,
	flipper_detach,
	flipper_exit,
	E_OK,
	1,
	NULL
};

struct _lf_device *lf_device_create(const char *name, struct _lf_endpoint *endpoint) {
	struct _lf_device *device = (struct _lf_device *)calloc(1, sizeof(struct _lf_device));
	lf_assert(device, failure, E_MALLOC, "");
	lf_assert(strlen(name) < sizeof(device -> configuration.name), failure, E_NAME, "The name '%s' is too long. Please choose a name with %lu characters or less.", name, sizeof(device -> configuration.name));
	strcpy(device -> configuration.name, name);
	device -> configuration.identifier = lf_crc((void *)name, (lf_size_t)strlen(name));
	device -> endpoint = endpoint;
	return device;
failure:
	if (device) free(device);
	return NULL;
}

/* Returns the active device. */
struct _lf_device *lf_get_current_device(void) {
    lf_assert(lf_current_device, failure, E_NULL, "NULL");
    return lf_current_device;
failure:
	return NULL;
}

/* Detaches a device from libflipper. */
int lf_detach(struct _lf_device *device) {
    lf_assert(device, failure, E_NULL, "NULL");
    /* Release the device's endpoint. */
    lf_endpoint_release(device -> endpoint);
    return lf_success;
failure:
	return lf_error;
}

/* Attempts to attach to all unattached devices. Returns how many devices were attached. */
int lf_attach(void) {
    return 0;
}

/* Registers an endpoint with the libflipper over which devices may be attached. */
int lf_register_endpoint(struct _lf_endpoint *endpoint) {
    return lf_success;
}

/* Deactivates libflipper state and releases the event loop. */
void lf_finish(void) {
    /* Release all of the events. */
    lf_ll_release(&lf_get_event_list());
    /* Release all of the attached devices. */
    lf_ll_release(&lf_get_device_list());
}

struct _lf_device *flipper_attach(void) {
	/* Attach a device over USB with the factory default name. */
	return flipper_attach_usb(LF_DEFAULT_NAME);
}

/* Attaches a USB device to the bridge endpoint. */
struct _lf_device *flipper_attach_usb(const char *name) {
#ifndef __lf_disable_usb__
	/* Make a backup of the slected device. */
	struct _lf_device *_device = flipper.device;
	/* Create a device with the name provided. */
	struct _lf_device *device = lf_device_create(name, &lf_bridge_ep);
	if (!device) {
		return NULL;
	}
	/* Set the current device. */
	flipper.device = device;
	/* Configure the device's endpoint. */
	if (device -> endpoint -> configure(device) < lf_success) {
		lf_error_raise(E_ENDPOINT, error_message("Failed to initialize bridge endpoint for usb device."));
		/* Detach the device in the event of an endpoint configuration failure. */
		flipper_detach(device);
		/* Restore the previously selected device. */
		flipper.device = _device;
		return NULL;
	}
	return device;
#else
	lf_error_raise(E_LIBUSB, error_message("USB devices are not supported on this platform."));
	return NULL;
#endif
}

struct _lf_device *flipper_attach_network(const char *name, const char *hostname) {
	struct _lf_device *device = lf_device_create(name, &lf_network_ep);
	if (!device) {
		return NULL;
	}
	if (device -> endpoint -> configure(device -> endpoint, hostname) < lf_success) {
		lf_error_raise(E_ENDPOINT, error_message("Failed to initialize endpoint for networked Flipper device."));
		/* Detach the device in the event of an endpoint configuration failure. */
		flipper_detach(device);
		return NULL;
	}
	/* Set the current device. */
	flipper.device = device;
	return device;
}

int flipper_select(struct _lf_device *device) {
	if (!device) {
		lf_error_raise(E_NULL, error_message("No device provided for selection."));
		return lf_error;
	}
	flipper.device = device;
	return lf_success;
}

int flipper_detach(struct _lf_device *device) {
	if (!device) {
		lf_error_raise(E_NULL, error_message("No device provided for release."));
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
		/* If the device has an endpoint, destroy it. */
		if (flipper.device -> endpoint) {
			flipper.device -> endpoint -> destroy(flipper.device -> endpoint);
		}
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
	/* Obtain the configuration from the device. */
	struct _lf_configuration configuration;
	_e = device -> endpoint -> pull(device -> endpoint, &configuration, sizeof(struct _lf_configuration));
	if (_e < lf_success) {
		return lf_error;
	}
	/* Obtain the result of the operation. */
	struct _fmr_result result;
	_e = lf_get_result(device, &result);
	if (_e < lf_success) {
		return lf_error;
	}
	/* Compare the device identifiers. */
	if (device -> configuration.identifier != configuration.identifier) {
		lf_error_raise(E_NO_DEVICE, error_message("Identifier mismatch for device '%s'. (0x%04x instead of 0x%04x)", device -> configuration.name, configuration.identifier, device -> configuration.identifier));
		return lf_error;
	}
	/* Copy the returned configuration into the device. */
	memcpy(&(device -> configuration), &configuration, sizeof(struct _lf_configuration));
	return lf_success;
}

/* Binds the lf_module structure to its counterpart on the attached device. */
int lf_bind(struct _lf_module *module) {
	/* Ensure that the module structure was allocated successfully. */
	if (!module) {
		lf_error_raise(E_NULL, error_message("No module provided to bind."));
		return lf_error;
	}
	/* Calculate the identifier of the module, including the NULL terminator. */
	lf_crc_t identifier = lf_crc(module -> name, strlen(module -> name) + 1);
	/* Attempt to get the module index. */
	fmr_module index = fld_index(identifier) | FMR_USER_INVOCATION_BIT;
	/* Throw an error if there is no counterpart module found. */
	lf_assert(index == -1, failure, E_MODULE, "No counterpart module loaded for bind to module '%s'.", module -> name);
	/* Set the module's indentifier. */
	module -> identifier = identifier;
	/* Set the module's index. */
	module -> index = index;
	/* Set the module's device. */
	module -> device = &flipper.device;
	return lf_success;
failure:
	return lf_error;
}

/* PROTOTYPE FUNCTION: Load an image into a device's RAM. */
int lf_ram_load(struct _lf_device *device, void *source, lf_size_t length) {
	if (!source) {
		lf_error_raise(E_NULL, error_message("No source provided for load operation."));
	} else if (!length) {
		return lf_success;
	}
	/* If no device is provided, throw an error. */
	if (!device) {
		lf_error_raise(E_NO_DEVICE, error_message("Failed to load to device."));
		return lf_error;
	}
	struct _fmr_packet _packet = { 0 };
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	/* Set the magic number. */
	_packet.header.magic = FMR_MAGIC_NUMBER;
	/* Compute the initial length of the packet. */
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	/* Set the packet class. */
	_packet.header.class = fmr_ram_load_class;
	/* Set the push length. */
	packet -> length = length;
	/* Compute and store the packet checksum. */
	_packet.header.checksum = lf_crc(packet, _packet.header.length);
	/* Send the packet to the target device. */
	int _e = lf_transfer(device, &_packet);
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
	return result.value;
}

/* Debugging functions for displaying the contents of various FMR related data structures. */

void lf_debug_call(struct _fmr_invocation *call) {
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
				lf_debug_call(&invocation -> call);
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
