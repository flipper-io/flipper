#include <flipper.h>

lf_device_list lf_attached_devices;
struct _lf_device *lf_current_device;

void lf_set_current_device(struct _lf_device *device) {
	lf_current_device = device;
}

struct _lf_device *lf_get_current_device(void) {
	return lf_current_device;
}

int lf_attach(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "Attempt to attach an invalid device.");

	lf_ll_append(&lf_attached_devices, device, lf_device_release);
	lf_select(device);
	return lf_success;

failure:
	return lf_error;
}

int lf_select(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "NULL device pointer provided for selection.");

	lf_set_current_device(device);
	return lf_success;

failure:
	return lf_error;
}

int lf_detach(struct _lf_device *device) {
	lf_assert(device, failure, E_NULL, "Invalid device provided to detach.");

	lf_ll_remove(&lf_attached_devices, device);
	return lf_success;

failure:
	return lf_error;
}

int __attribute__((__destructor__)) lf_exit(void) {
	lf_ll_release(&lf_attached_devices);
	return lf_success;
}

lf_return_t lf_invoke(struct _lf_device *device, char *module, lf_function function, lf_type ret, struct _lf_ll *args) {
	lf_assert(device, failure, E_NULL, "NULL device was provided to '%s'.", __FUNCTION__);
	lf_assert(module, failure, E_NULL, "NULL module was provided to '%s'.", __FUNCTION__);

	struct _fmr_packet _packet;
	int e;
	struct _fmr_result result;

	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_invocation_packet);

	struct _lf_module *m = dyld_module(device, module);
	lf_assert(m, failure, E_MODULE, "No counterpart found for module '%s'.", module);

	struct _fmr_invocation_packet *packet = (struct _fmr_invocation_packet *)(&_packet);
	e = lf_create_call(m->idx, function, ret, args, &_packet.header, &packet->call);
	lf_assert(e == lf_success, failure, E_NULL, "Failed to generate a valid call to module '%s'.", module);
	_packet.header.checksum = lf_crc(&_packet, _packet.header.length);
	lf_debug_packet(&_packet, sizeof(struct _fmr_packet));

	e = device->write(device, &_packet, sizeof(struct _fmr_packet));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to send message to device '%s'.", device->name);

	e = device->read(device, &result, sizeof(struct _fmr_result));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to receive message from the device '%s'.", device->name);
	lf_debug_result(&result);
	lf_assert(result.error == E_OK, failure, result.error, "An error occured on the device '%s':", device->name);
	return result.value;

failure:
	return lf_error;
}

lf_return_t lf_push(struct _lf_device *device, void *dst, void *src, size_t len) {
	lf_assert(device, failure, E_NULL, "NULL device was provided to '%s'.", __FUNCTION__);
	lf_assert(dst, failure, E_NULL, "NULL dst was provided to '%s'.", __FUNCTION__);
	lf_assert(src, failure, E_NULL, "NULL src was provided to '%s'.", __FUNCTION__);
	lf_assert(len, failure, E_NULL, "Zero length provided to '%s'.", __FUNCTION__);

	struct _fmr_packet _packet;
	int e;
	struct _fmr_result result;

	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	_packet.header.type = fmr_push_class;

	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	packet->len = len;
	packet->ptr = dst;
	_packet.header.checksum = lf_crc(&_packet, _packet.header.length);
	lf_debug_packet(&_packet, sizeof(struct _fmr_packet));

	e = device->write(device, &_packet, sizeof(struct _fmr_packet));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to send message to device '%s'.", device->name);
	e = device->write(device, src, len);
	lf_assert(e == lf_success, failure, E_FMR, "Failed to push data to device '%s'.", device->name);

	e = device->read(device, &result, sizeof(struct _fmr_result));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to receive message from the device '%s'.", device->name);
	lf_debug_result(&result);
	lf_assert(result.error == E_OK, failure, result.error, "An error occured on the device '%s':", device->name);

	return lf_success;
failure:
	return lf_error;
}

lf_return_t lf_pull(struct _lf_device *device, void *dst, void *src, size_t len) {
	lf_assert(device, failure, E_NULL, "NULL device was provided to '%s'.", __FUNCTION__);
	lf_assert(dst, failure, E_NULL, "NULL dst was provided to '%s'.", __FUNCTION__);
	lf_assert(src, failure, E_NULL, "NULL src was provided to '%s'.", __FUNCTION__);
	lf_assert(len, failure, E_NULL, "Zero length provided to '%s'.", __FUNCTION__);

	struct _fmr_packet _packet;
	int e;
	struct _fmr_result result;

	memset(&_packet, 0, sizeof(struct _fmr_packet));
	_packet.header.magic = FMR_MAGIC_NUMBER;
	_packet.header.length = sizeof(struct _fmr_push_pull_packet);
	_packet.header.type = fmr_pull_class;
	struct _fmr_push_pull_packet *packet = (struct _fmr_push_pull_packet *)(&_packet);
	packet->len = len;
	packet->ptr = src;
	_packet.header.checksum = lf_crc(&_packet, _packet.header.length);
	lf_debug_packet(&_packet, sizeof(struct _fmr_packet));

	e = device->write(device, &_packet, sizeof(struct _fmr_packet));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to send message to device '%s'.", device->name);
	e = device->read(device, dst, len);
	lf_assert(e == lf_success, failure, E_FMR, "Failed to pull data from device '%s'.", device->name);

	e = device->read(device, &result, sizeof(struct _fmr_result));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to receive message from the device '%s'.", device->name);
	lf_debug_result(&result);
	lf_assert(result.error == E_OK, failure, result.error, "An error occured on the device '%s':", device->name);

	return lf_success;
failure:
	return lf_error;
}

int lf_dyld(struct _lf_device *device, char *module) {
	lf_assert(module, failure, E_NULL, "NULL module provided to '%s'.", __FUNCTION__);
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
	_packet.header.checksum = lf_crc(&_packet, _packet.header.length);
	lf_debug_packet(&_packet, sizeof(struct _fmr_packet));

	e = device->write(device, &_packet, sizeof(struct _fmr_packet));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to send message to device '%s'.", device->name);

	e = device->read(device, &result, sizeof(struct _fmr_result));
	lf_assert(e == lf_success, failure, E_ENDPOINT, "Failed to receive message from the device '%s'.", device->name);
	lf_debug_result(&result);
	lf_assert(result.error == E_OK, failure, result.error, "An error occured on the device '%s':", device->name);
	return result.value;

failure:
	return lf_error;
}
