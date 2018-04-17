#ifndef __lf_device_h__
#define __lf_device_h__

#include <flipper/types.h>
#include <flipper/error.h>

/* Macros that quantify device attributes. */
#define lf_device_8bit (1 << 1)
#define lf_device_16bit (1 << 2)
#define lf_device_32bit (1 << 3)
#define lf_device_big_endian    1
#define lf_device_little_endian 0

/* Describes a device capible of responding to FMR packets. */
struct _lf_device {
	/* The human readable name of the device. */
	char *name;
	/* The device's firmware version. */
	lf_version_t version;
	/* The modules loaded on the device. */
	struct _lf_ll *modules;
	/* Receives arbitrary data from the device. */
	int (* read)(struct _lf_device *device, void *dst, size_t length);
	/* Transmits arbitrary data to the device. */
	int (* write)(struct _lf_device *device, void *src, size_t length);
	/* Releases device state. */
	int (* release)(struct _lf_device *device);
	/* The device's context. */
	void *_dev_ctx;
	/* The endpoint's context. */
	void *_ep_ctx;
};

struct _lf_device *lf_device_create(int (* read)(struct _lf_device *device, void *dst, size_t length),
									int (* write)(struct _lf_device *device, void *src, size_t length),
									int (* release)(struct _lf_device *device));
int lf_device_release(struct _lf_device *device);

#endif
