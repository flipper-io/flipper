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
	/* A pointer to the endpoint through which packets will be transferred. */
	struct _lf_endpoint *endpoint;
	/* The device's context. */
	void *_ctx;
	/* The modules loaded on the device. */
	struct _lf_ll *modules;
};

struct _lf_device *lf_device_create(char *name, struct _lf_endpoint *endpoint);
int lf_device_release(struct _lf_device *device);

#endif
