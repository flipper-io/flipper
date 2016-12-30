#define __private_include__
#include <flipper/flipper.h>
#include <flipper/carbon/swd.h>

int swd_configure(void) {
	return lf_invoke(&_swd, _swd_configure, NULL);
}
