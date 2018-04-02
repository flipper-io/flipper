#ifndef __lf_endpoint_h__
#define __lf_endpoint_h__

#include <flipper/types.h>
#include <flipper/device.h>

/* Standardizes interaction with a physical hardware bus for the transmission of arbitrary data. */
struct _lf_endpoint {
	/* Reconfigures the endpoint with a new context. */
	int (* configure)(struct _lf_device *device, void *_ctx);
	/* Indicates whether or not the endpoint is ready to send or receive data. */
	bool (* ready)(struct _lf_device *device);
	/* Transmits a block of data through the endpoint. */
	int (* push)(struct _lf_device *device, void *source, lf_size_t length);
	/* Receives a block of data from the endpoint. */
	int (* pull)(struct _lf_device *device, void *destination, lf_size_t length);
	/* Destroys any state associated with the endpoint. */
	int (* destroy)(struct _lf_endpoint *endpointc);
	/* Tracks endpoint specific context. */
	void *_ctx;
};

struct _lf_endpoint *lf_endpoint_create(int (* configure)(struct _lf_device *device, void *ctx),
										bool (* ready)(struct _lf_device *device),
										int (* push)(struct _lf_device *device, void *source, lf_size_t length),
										int (* pull)(struct _lf_device *device, void *destination, lf_size_t length),
										int (* destroy)(struct _lf_endpoint *endpoint),
										size_t ctx_size);
int lf_endpoint_release(struct _lf_endpoint *endpoint);

#endif
