#include <flipper/libflipper.h>

#ifdef __use_dac__
#define __private_include__
#include <flipper/dac.h>

LF_MODULE(_dac, "dac", "Provides access to the device's digital to analog converter.", _dac_id);

/* Define the virtual interface for this module. */
const struct _dac dac = {
	dac_configure
};

LF_WEAK int dac_configure(void) {
	return lf_invoke(&_dac, _dac_configure, NULL);
}

#endif
