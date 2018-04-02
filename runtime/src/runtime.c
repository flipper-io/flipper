#include <flipper.h>

lf_return_t lf_invoke(struct _lf_device *device, char *module, lf_function function, lf_type ret, struct _lf_ll *parameters) {
	lf_assert(device, failure, E_NULL, "No device was provided to '%s'.", __PRETTY_FUNCTION__);
	lf_assert(module, failure, E_NULL, "No module was provided to '%s'.", __PRETTY_FUNCTION__);

	struct _fmr_packet _packet;
	int e;
	struct _fmr_result result;

	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_invocation_packet);

	struct _lf_module *m = dyld_module(device, module);
	lf_assert(m, failure, E_MODULE, "No counterpart found for module '%s'.", module);

	struct _fmr_invocation_packet *packet = (struct _fmr_invocation_packet *)(&_packet);
	e = lf_create_call(m->idx, function, ret, parameters, &_packet.header, &packet->call);
	lf_assert(e == lf_success, failure, E_NULL, "Failed to generate a valid call to module '%s'.", module);
	_packet.header.checksum = lf_crc(&_packet, _packet.header.length);
	lf_debug_packet(&_packet, sizeof(struct _fmr_packet));

	e = device->endpoint->push(device, &_packet, sizeof(struct _fmr_packet));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to send message to device '%s'.", device->name);

	e = device->endpoint->pull(device, &result, sizeof(struct _fmr_result));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to receive message from the device '%s'.", device->name);
	lf_debug_result(&result);
	lf_assert(result.error == E_OK, failure, result.error, "An error occured on the device '%s':", device->name);
	return result.value;

failure:
	return lf_error;
}

lf_return_t lf_push(struct _lf_device *device, char *module, lf_function function, void *source, lf_size_t length, struct _lf_ll *parameters) {
	lf_assert(device, failure, E_NULL, "No device was provided to '%s'.", __PRETTY_FUNCTION__);
	lf_assert(module, failure, E_NULL, "No module was provided to '%s'.", __PRETTY_FUNCTION__);
	if (!length) return lf_success;

	struct _fmr_packet _packet;
	int e;
	struct _fmr_result result;

	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	_packet.header.type = fmr_push_class;
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	packet->length = length;

	struct _lf_module *m = dyld_module(device, module);
	lf_assert(m, failure, E_MODULE, "No counterpart found for module '%s'.", module);

	e = lf_create_call(m->idx, function, lf_int_t, lf_args(lf_ptr(source), lf_infer(length)), &_packet.header, &packet->call);
	lf_assert(e == lf_success, failure, E_NULL, "Failed to generate a valid push to module '%s'.", module);
	_packet.header.checksum = lf_crc(packet, _packet.header.length);
	lf_debug_packet(&_packet, sizeof(struct _fmr_packet));

	e = device->endpoint->push(device, packet, sizeof(struct _fmr_packet));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to send message to device '%s'.", device->name);
	e = device->endpoint->push(device, source, length);
	lf_assert(e == lf_success, failure, E_FMR, "Failed to push data to device '%s'.", device->name);

	e = device->endpoint->pull(device, &result, sizeof(struct _fmr_result));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to receive message from the device '%s'.", device->name);
	lf_debug_result(&result);
	lf_assert(result.error == E_OK, failure, result.error, "An error occured on the device '%s':", device->name);
	return result.value;

failure:
	return lf_error;
}

lf_return_t lf_pull(struct _lf_device *device, char *module, lf_function function, void *destination, lf_size_t length, struct _lf_ll *parameters) {
	lf_assert(device, failure, E_NULL, "No device was provided to '%s'.", __PRETTY_FUNCTION__);
	lf_assert(module, failure, E_NULL, "No module was provided to '%s'.", __PRETTY_FUNCTION__);
	if (!length) return lf_success;

	struct _fmr_packet _packet;
	int e;
	struct _fmr_result result;

	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	_packet.header.type = fmr_pull_class;
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	packet->length = length;

	struct _lf_module *m = dyld_module(device, module);
	lf_assert(m, failure, E_MODULE, "No counterpart found for module '%s'.", module);

	e = lf_create_call(m->idx, function, lf_int_t, lf_args(lf_ptr(destination), lf_infer(length)), &_packet.header, &packet->call);
	lf_assert(e == lf_success, failure, E_NULL, "Failed to generate a valid pull from module '%s'.", module);
	_packet.header.checksum = lf_crc(packet, _packet.header.length);
	lf_debug_packet(&_packet, sizeof(struct _fmr_packet));

	e = device->endpoint->push(device, packet, sizeof(struct _fmr_packet));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to send message to device '%s'.", device->name);
	e = device->endpoint->pull(device, destination, length);
	lf_assert(e == lf_success, failure, E_FMR, "Failed to pull data from devie '%s'.", device->name);

	e = device->endpoint->pull(device, &result, sizeof(struct _fmr_result));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to receive message from the device '%s'.", device->name);
	lf_debug_result(&result);
	lf_assert(result.error == E_OK, failure, result.error, "An error occured on the device '%s':", device->name);
	return result.value;

failure:
	return lf_error;
}

/* Asks the device's dynamic loader for a module index. */
int lf_dyld(struct _lf_device *device, char *module) {
	lf_assert(module, failure, E_NULL, "No module provided to '%s'.", __PRETTY_FUNCTION__);
	lf_assert(strlen(module) < 16, failure, E_OVERFLOW, "Module name '%s' is invalid. Module names must be 16 characters or less.", module);

	struct _fmr_packet _packet;
	int e;
	struct _fmr_result result;

	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_dyld_packet);
	_packet.header.type = fmr_dyld_class;

	struct _fmr_dyld_packet *packet = (struct _fmr_dyld_packet *)(&_packet);
	strcpy(packet->module, module);
	_packet.header.checksum = lf_crc(packet, _packet.header.length);
	lf_debug_packet(&_packet, sizeof(struct _fmr_packet));

	e = device->endpoint->push(device, packet, sizeof(struct _fmr_packet));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to send message to device '%s'.", device->name);

	e = device->endpoint->pull(device, &result, sizeof(struct _fmr_result));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to receive message from the device '%s'.", device->name);
	lf_debug_result(&result);
	lf_assert(result.error == E_OK, failure, result.error, "An error occured on the device '%s':", device->name);
	return result.value;

failure:
	return lf_error;
}
