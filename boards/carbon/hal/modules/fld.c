#define __private_include__
#include <flipper/flipper.h>
#include <flipper/carbon/fld.h>

int fld_configure(void) {
	return lf_invoke(&_fld, _fld_configure, NULL);
}
