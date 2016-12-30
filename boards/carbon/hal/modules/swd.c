#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/swd.h>

int swd_configure(void) {
	return lf_invoke(&_swd, _swd_configure, NULL);
}
