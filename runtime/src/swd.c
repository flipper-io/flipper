#include <flipper/swd.h>

#ifdef __use_swd__

LF_MODULE(_swd, "swd", "Interfaces with the device's single wire debug unit.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _swd_interface swd = {
	swd_configure
};

LF_WEAK int swd_configure(void) {
	return lf_invoke(lf_get_current_device(), &_swd, _swd_configure, lf_int_t, NULL);
}

#endif
