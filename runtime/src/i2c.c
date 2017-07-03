#include <flipper/libflipper.h>

#ifdef __use_i2c__
#define __private_include__
#include <flipper/i2c.h>

LF_MODULE(_i2c, "i2c", "Interfaces with the device's I2C peripheral.", _i2c_id);

/* Define the virtual interface for this module. */
const struct _i2c i2c = {
	i2c_configure
};

LF_WEAK int i2c_configure(void) {
	return lf_invoke(&_i2c, _i2c_configure, NULL);
}

#endif
