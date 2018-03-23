#include <flipper.h>

/* Creates a new libflipper device. */
struct _lf_device *lf_device_create(char *name, struct _lf_endpoint *endpoint) {
	struct _lf_device *device = NULL;
	lf_assert(name, failure, E_NULL, "NULL name string provided to '%s'.", __PRETTY_FUNCTION__);
	lf_assert(endpoint, failure, E_NULL, "NULL endpoint provided to '%s'.", __PRETTY_FUNCTION__);
	device = (struct _lf_device *)calloc(1, sizeof(struct _lf_device));
	lf_assert(device, failure, E_MALLOC, "Failed to allocate memory for new device.");
	device->name = strdup(name);
	device->endpoint = endpoint;
	return device;
failure:
	free(device);
	return NULL;
}

int lf_device_release(struct _lf_device *device) {
    lf_assert(device, failure, E_NULL, "NULL device provided to '%s'", __PRETTY_FUNCTION__);
	lf_endpoint_release(device->endpoint);
	free(device->name);
	free(device);
	return lf_success;
failure:
    return lf_error;
}
