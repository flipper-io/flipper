#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/fld.h>

int fld_configure(void) {
	return lf_invoke(&_fld, _fld_configure, NULL);
}
