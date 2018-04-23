#include <flipper.h>

enum { _dac_configure };

int dac_configure(void);

void *dac_interface[] = {
	&dac_configure
};

LF_MODULE(dac, "dac", dac_interface);

LF_WEAK int dac_configure(void) {
	lf_return_t retval;
	lf_invoke(lf_get_current_device(), "dac", _dac_configure, lf_int_t, &retval, NULL);
	return (int)retval;
}

