#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_temp, "temp", "Interfaces with the device's temperature sensor.", _temp_id);

int temp_configure(void) {
	return lf_invoke(&_temp, _temp_configure, NULL);
}
