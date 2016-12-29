#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_dac, "dac", "Provides access to the device's digital to analog converter.", _dac_id);

int dac_configure(void) {
	return lf_invoke(&_dac, _dac_configure, NULL);
}
