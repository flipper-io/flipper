#include <flipper.h>

/* Creates a new libflipper device. */
struct _lf_device *lf_device_create(int (* read)(struct _lf_device *device, void *dst, uint32_t length),
									int (* write)(struct _lf_device *device, void *src, uint32_t length),
									int (* release)(struct _lf_device *device)) {
	struct _lf_device *device = NULL;
	device = (struct _lf_device *)calloc(1, sizeof(struct _lf_device));
	lf_assert(device, fail, E_MALLOC, "Failed to allocate memory for new device.");
	device->read = read;
	device->write = write;
	device->release = release;
	return device;
fail:
	free(device);
	return NULL;
}

int lf_device_release(struct _lf_device *device) {
    lf_assert(device, fail, E_NULL, "NULL device provided to '%s'", __PRETTY_FUNCTION__);
	free(device->name);
	free(device);
	return lf_success;
fail:
    return lf_error;
}
