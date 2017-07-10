#include <flipper/libflipper.h>

struct _lf_device *_atmegau2_create(struct _lf_endpoint *endpoint) {
	return lf_device_create(endpoint);
}
