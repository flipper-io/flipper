#define __private_include__
#include <flipper/temp.h>

#ifdef __use_temp__

LF_MODULE(_temp, "temp", "Interfaces with the device's temperature sensor.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _temp_interface temp = {
	temp_configure
};

LF_WEAK int temp_configure(void) {
	return lf_invoke(&_temp, _temp_configure, lf_int_t, NULL);
}

#endif
