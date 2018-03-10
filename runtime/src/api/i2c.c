#include <flipper/i2c.h>

#ifdef __use_i2c__

LF_MODULE(_i2c, "i2c", "Interfaces with the device's I2C peripheral.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _i2c_interface i2c = {
	i2c_configure
};

LF_WEAK int i2c_configure(void) {
	return lf_invoke(lf_get_current_device(), &_i2c, _i2c_configure, lf_int_t, NULL);
}

#endif
