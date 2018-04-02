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
	/* Release all of the attached devices. */
	lf_ll_release(&lf_attached_devices);
	return lf_success;
}
