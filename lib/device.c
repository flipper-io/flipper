#include "libflipper.h"

/* Creates a new libflipper device. */
struct _lf_device *lf_device_create(int (*read)(struct _lf_device *device, void *dst, uint32_t length), int (*write)(struct _lf_device *device, void *src, uint32_t length),
                                    int (*release)(void *device)) {
    struct _lf_device *device = NULL;
    device = (struct _lf_device *)calloc(1, sizeof(struct _lf_device));
    lf_assert(device, E_MALLOC, "Failed to allocate memory for new device.");
    device->read = read;
    device->write = write;
    device->release = release;
    return device;
fail:
    free(device);
    return NULL;
}

void lf_device_release(void *_device) {
    struct _lf_device *device = (struct _lf_device *)_device;
    lf_assert(device, E_NULL, "invalid device");
    free(device);
fail:
    return;
}
