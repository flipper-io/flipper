#include <flipper/libflipper.h>

#ifdef __use_swd__
#define __private_include__
#include <flipper/swd.h>

LF_MODULE(_swd, "swd", "Interfaces with the device's single wire debug unit.", _swd_id);

/* Define the virtual interface for this module. */
const struct _swd swd = {
	swd_configure
};

LF_WEAK int swd_configure(void) {
	return lf_invoke(&_swd, _swd_configure, NULL);
}

#endif
