#include <flipper/libflipper.h>

#ifdef __use_temp__
#define __private_include__
#include <flipper/temp.h>

LF_MODULE(_temp, "temp", "Interfaces with the device's temperature sensor.", _temp_id);

/* Define the virtual interface for this module. */
const struct _temp temp = {
	temp_configure
};

LF_WEAK int temp_configure(void) {
	return lf_invoke(&_temp, _temp_configure, NULL);
}

#endif
