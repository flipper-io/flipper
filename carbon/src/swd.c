#include <flipper.h>

enum { _swd_configure };

int swd_configure(void);

void *swd_interface[] = {
	&swd_configure
};

LF_MODULE(swd, "swd", swd_interface);

LF_WEAK int swd_configure(void) {
	return lf_invoke(lf_get_current_device(), "swd", _swd_configure, lf_int_t, NULL);
}

