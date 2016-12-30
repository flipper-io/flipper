#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/modules/temp.h>

int temp_configure(void) {
	return lf_invoke(&_temp, _temp_configure, NULL);
}
