#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/fmr.h>

/* Include the Carbon board file. */
#include <flipper/carbon.h>

int lf_get_result(struct _lf_device *device, struct _fmr_result *result) {
	/* Obtain the response packet from the device. */
	int _e = lf_retrieve(device, result);
	lf_debug_result(result);
	lf_assert(_e == lf_success, failure, E_ENDPOINT, "Failed to obtain response from device '%s':", device->configuration.name);
	lf_assert(result->error == E_OK, failure, result->error, "An error occured on the device '%s':", device->configuration.name);
	return lf_success;
failure:
	return lf_error;
}

int lf_transfer(struct _lf_device *device, struct _fmr_packet *packet) {
	lf_debug_packet(packet, sizeof(struct _fmr_packet));
	int _e = device->endpoint->push(device->endpoint, packet, sizeof(struct _fmr_packet));
	lf_assert(_e == lf_success, failure, E_ENDPOINT, "Failed to transfer packet to device '%s'.", device->configuration.name);
	return lf_success;
failure:
	return lf_error;
}

int lf_retrieve(struct _lf_device *device, struct _fmr_result *result) {
	int _e = device->endpoint->pull(device->endpoint, result, sizeof(struct _fmr_result));
	lf_assert(_e == lf_success, failure, E_ENDPOINT, "Failed to retrieve packet from the device '%s'.", device->configuration.name);
	return lf_success;
failure:
	return lf_error;
}

lf_return_t lf_invoke(struct _lf_module *module, fmr_function function, fmr_type ret, struct _lf_ll *parameters) {
	lf_assert(module, failure, E_NULL, "No module was specified for function invocation.");
	lf_assert(module->index != -1, failure, E_MODULE, "The module '%s' has not been configured. Call '%s_configure()' first.", module->name, module->name);
	lf_assert(module->device, failure, E_NO_DEVICE, "The module '%s' has no target device. Did you attach before configuring?", module->name);

	/* The raw packet into which the invocation information will be loaded .*/
	struct _fmr_packet _packet;
	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_invocation_packet);

	#warning Remove this.
	/* If the user module bit is set, make the invocation a user invocation. */
	if (module->index & FMR_USER_INVOCATION_BIT) {
		_packet.header.class = fmr_user_invocation_class;
	} else {
		/* Otherwise, make it a standard invocation. */
		_packet.header.class = fmr_standard_invocation_class;
	}

	/* Generate the function call in the outgoing packet. */
	struct _fmr_invocation_packet *packet = (struct _fmr_invocation_packet *)(&_packet);
	int _e = fmr_create_call((uint8_t)(module->index), function, ret, parameters, &_packet.header, &packet->call);
	lf_assert(_e == lf_success, failure, E_NULL, "Failed to generate a valid call to module '%s'.", module->name);
	_packet.header.checksum = lf_crc(&_packet, _packet.header.length);

	_e = lf_transfer(module->device, &_packet);
	lf_assert(_e == lf_success, failure, E_FMR, "Failed to transfer command to module '%s'.", module->name);

	struct _fmr_result result;
	lf_get_result(module->device, &result);
	return result.value;

failure:
	return -1;
}

/* Sends data to the address space of the device for a function call. Deallocated at the function return. */
fmr_va fmr_data(void *data, lf_size_t size) {
	return fmr_ptr(NULL);
}

lf_return_t lf_push(struct _lf_module *module, fmr_function function, void *source, lf_size_t length, struct _lf_ll *parameters) {
	lf_assert(module, failure, E_NULL, "NULL module was specified for data push.");
	lf_assert(module->index != -1, failure, E_MODULE, "The module '%s' has not been configured. Call '%s_configure()' first.", module->name, module->name);
	lf_assert(module->device, failure, E_NO_DEVICE, "The module '%s' has no target device. Did you attach before configuring?", module->name);
	if (!length) return lf_success;

	struct _fmr_packet _packet;
	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	_packet.header.class = fmr_push_class;
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	packet->length = length;

	int _e = fmr_create_call(module->index, function, fmr_int_t, fmr_args(fmr_ptr(source), fmr_infer(length)), &_packet.header, &packet->call);
	lf_assert(_e == lf_success, failure, E_NULL, "Failed to generate a valid push to module '%s'.", module->name);
	_packet.header.checksum = lf_crc(packet, _packet.header.length);

	/* Send the packet to the target device. */
	_e = lf_transfer(module->device, &_packet);
	lf_assert(_e == lf_success, failure, E_FMR, "Failed to transfer push command to module '%s'.", module->name);

	/* Transfer the data through to the address space of the device. */
	_e = module->device->endpoint->push(module->device->endpoint, source, length);
	lf_assert(_e == lf_success, failure, E_FMR, "Failed to push data to module '%s'.", module->name);

	struct _fmr_result result;
	lf_get_result(module->device, &result);
	return result.value;

failure:
	return -1;
}

lf_return_t lf_pull(struct _lf_module *module, fmr_function function, void *destination, lf_size_t length, struct _lf_ll *parameters) {
	lf_assert(module, failure, E_NULL, "NULL module was specified for data pull.");
	lf_assert(module->index != -1, failure, E_MODULE, "The module '%s' has not been configured. Call '%s_configure()' first.", module->name, module->name);
	lf_assert(module->device, failure, E_NO_DEVICE, "The module '%s' has no target device. Did you attach before configuring?", module->name);
	if (!length) return lf_success;

	struct _fmr_packet _packet;
	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	_packet.header.class = fmr_pull_class;
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	packet->length = length;

	/* Generate the function call in the outgoing packet. */
	int _e = fmr_create_call(module->index, function, fmr_int_t, fmr_args(fmr_ptr(destination), fmr_infer(length)), &_packet.header, &packet->call);
	lf_assert(_e == lf_success, failure, E_NULL, "Failed to generate a valid pull from module '%s'.", module->name);
	_packet.header.checksum = lf_crc(packet, _packet.header.length);

	/* Send the packet to the target device. */
	_e = lf_transfer(module->device, &_packet);
	lf_assert(_e == lf_success, failure, E_FMR, "Failed to transfer pull command to module '%s'.", module->name);

	/* Obtain the data from the address space of the device. */
	_e = module->device->endpoint->pull(module->device->endpoint, destination, length);
	lf_assert(_e == lf_success, failure, E_FMR, "Failed to pull data from module '%s'.", module->name);

	struct _fmr_result result;
	lf_get_result(module->device, &result);
	return result.value;

failure:
	return -1;
}

int lf_load(struct _lf_device *device, void *source, lf_size_t length) {
	lf_assert(device, failure, E_NULL, "No device specified for RAM load.");
	lf_assert(source, failure, E_NULL, "No source specified for RAM load to device '%s'.", device->configuration.name);
	lf_assert(length, failure, E_NULL, "No length specified for RAM load to device '%s'.", device->configuration.name);

	struct _fmr_packet _packet;
	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	_packet.header.class = fmr_ram_load_class;
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	packet->length = length;
	_packet.header.checksum = lf_crc(packet, _packet.header.length);

	/* Send the packet to the target device. */
	int _e = lf_transfer(device, &_packet);
	lf_assert(_e == lf_success, failure, E_FMR, "Failed to transfer load command to device '%s'.", device->configuration.name);

	/* Transfer the data through to the address space of the device. */
	_e = device->endpoint->push(device->endpoint, source, length);
	lf_assert(_e == lf_success, failure, E_FMR, "Failed to push image data to device '%s'.", device->configuration.name);

	struct _fmr_result result;
	lf_get_result(device, &result);
	return result.value;

failure:
	return -1;
}