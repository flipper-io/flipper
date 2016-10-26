#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_i2c, "i2c", "Interfaces with the device's I2C peripheral.", _i2c_id);

int i2c_configure(void) {
	return lf_invoke(&_i2c, _i2c_configure, NULL);
}
