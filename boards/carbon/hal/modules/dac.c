#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/dac.h>

int dac_configure(void) {
	return lf_invoke(&_dac, _dac_configure, NULL);
}
