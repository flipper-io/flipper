#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/fmr.h>

/* Include the Carbon board file. */
#include <flipper/carbon.h>

int lf_get_result(struct _lf_device *device, struct _fmr_result *result) {
	/* Obtain the response packet from the device. */
	int _e = lf_retrieve(device, result);
	if (_e < lf_success) {
		return lf_error;
	}
	lf_debug_result(result);
	/* If an error occured on the device, raise it. */
	if (result -> error != E_OK) {
		lf_error_raise(result -> error, error_message("An error occured on the device '%s':", device -> configuration.name));
		return lf_error;
	}
	return lf_success;
}

int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet) {
	lf_debug_packet(packet, sizeof(struct _fmr_packet));
	/* Transfer the packet buffer through its registered endpoint. */
	int _e = device -> endpoint -> push(device -> endpoint, packet, sizeof(struct _fmr_packet));
	/* Ensure that the packet was successfully transferred to the device. */
	if (_e < lf_success) {
		lf_error_raise(E_ENDPOINT, error_message("Failed to transfer packet to device '%s'.", device -> configuration.name));
		return lf_error;
	}
	return lf_success;
}

int lf_retrieve(struct _lf_device *device, struct _fmr_result *result) {
	/* Receive the packet through the device's endpoint. */
	int _e = device -> endpoint -> pull(device -> endpoint, result, sizeof(struct _fmr_result));
	/* Ensure that the packet was successfully obtained from the device. */
	if (_e < lf_success) {
		lf_error_raise(E_ENDPOINT, error_message("Failed to retrieve packet from the device '%s'.", device -> configuration.name));
		return lf_error;
	}
	return lf_success;
}

lf_return_t _lf_invoke(struct _lf_module *module, fmr_function function, struct _lf_ll *args) {
	lf_assert(module, failure, E_MODULE, "Attempt to invoke a function within an invalid module.");

	struct _lf_device *device = module->device;
	struct _fmr_invocation_packet packet;
	fmr_create_call(module->index, function, args, &packet.header, &packet.call);
	lf_transfer(device, (struct _fmr_packet *)&packet);


failure:
	return 0;
}

lf_return_t lf_invoke(struct _lf_module *module, fmr_function function, struct _lf_ll *parameters) {
	/* Ensure that the module pointer is valid. */
	if (!module) {
		lf_error_raise(E_NULL, error_message("No module was specified for function invocation."));
		return lf_error;
	}
	/* Obtain the module's target device. */
	struct _lf_device *device = module -> device;
	/* If no device is provided, raise an error. */
	if (!device) {
		lf_error_raise(E_NO_DEVICE, error_message("The module '%s' has no target device. Did you attach?", module -> name));
		return lf_error;
	}
	/* Ensure that the module has been bound. */
	if ((int8_t)(module -> index) == -1) {
		lf_error_raise(E_MODULE, error_message("The module '%s' has not been bound to a module on its device.", module -> name));
		return lf_error;
	}
	/* The raw packet into which the invocation information will be loaded .*/
	struct _fmr_packet _packet;
	/* A packet cast that exposes the data structures specific to this packet subclass. */
	struct _fmr_invocation_packet *packet = (struct _fmr_invocation_packet *)(&_packet);
	/* Set the magic number. */
	_packet.header.magic = FMR_MAGIC_NUMBER;
	/* Compute the initial length of the packet. */
	_packet.header.length = sizeof(struct _fmr_invocation_packet);

	#warning Remove this.
	/* If the user module bit is set, make the invocation a user invocation. */
	if (module -> index & FMR_USER_INVOCATION_BIT) {
		_packet.header.class = fmr_user_invocation_class;
	} else {
		/* Otherwise, make it a standard invocation. */
		_packet.header.class = fmr_standard_invocation_class;
	}

	/* Generate the function call in the outgoing packet. */
	int _e = fmr_create_call((uint8_t)(module -> index), function, parameters, &_packet.header, &packet -> call);
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

/* Sends data to the address space of the device for a function call. Deallocated at the function return. */
fmr_va fmr_data(void *data, lf_size_t size) {
	return fmr_ptr(NULL);
}

lf_return_t lf_push(struct _lf_module *module, fmr_function function, void *source, lf_size_t length, struct _lf_ll *parameters) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		lf_error_raise(E_NULL, error_message("No module specified for message runtime push to module '%s'.", module -> name));
		return lf_error;
	} else if (!source) {
		lf_error_raise(E_NULL, error_message("No source provided for message runtime push to module '%s'.", module -> name));
	} else if (!length) {
		return lf_success;
	}
	/* Obtain the target device from the module. */
	struct _lf_device *device = module -> device;
	/* If no device is provided, throw an error. */
	if (!device) {
		lf_error_raise(E_NO_DEVICE, error_message("Failed to push to device."));
		return lf_error;
	}
	struct _fmr_packet _packet;
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	/* Set the magic number. */
	_packet.header.magic = FMR_MAGIC_NUMBER;
	/* Compute the initial length of the packet. */
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	/* Set the packet class. */
	_packet.header.class = fmr_push_class;
	/* Set the push length. */
	packet -> length = length;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_create_call(module -> index, function, fmr_args(fmr_ptr(source), fmr_infer(length)), &_packet.header, &packet -> call);
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
	return result.value;
}

lf_return_t lf_pull(struct _lf_module *module, fmr_function function, void *destination, lf_size_t length, struct _lf_ll *parameters) {
	/* Ensure that we have a valid module and argument pointer. */
	if (!module) {
		lf_error_raise(E_NULL, error_message("No module specified for message runtime pull from module '%s'.", module -> name));
		return lf_error;
	} else if (!destination) {
		lf_error_raise(E_NULL, error_message("No destination provided for message runtime pull from module '%s'.", module -> name));
	} else if (!length) {
		return lf_success;
	}
	/* Obtain the target device from the module. */
	struct _lf_device *device = module -> device;
	/* If no device is provided, throw an error. */
	if (!device) {
		lf_error_raise(E_NO_DEVICE, error_message("Failed to pull from device."));
		return lf_error;
	}
	struct _fmr_packet _packet;
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	/* Set the magic number. */
	_packet.header.magic = FMR_MAGIC_NUMBER;
	/* Compute the initial length of the packet. */
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	/* Set the packet class. */
	_packet.header.class = fmr_pull_class;
	/* Set the pull length. */
	packet -> length = length;
	/* Generate the function call in the outgoing packet. */
	int _e = fmr_create_call(module -> index, function, fmr_args(fmr_ptr(destination), fmr_infer(length)), &_packet.header, &packet -> call);
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
	return result.value;
}
