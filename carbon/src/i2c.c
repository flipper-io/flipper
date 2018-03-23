#include <flipper.h>

enum { _i2c_stop, _i2c_write, _i2c_read, _i2c_configure, _i2c_start_read };

void i2c_stop(void);
void i2c_write(uint8_t byte);
uint8_t i2c_read(void);
int i2c_configure(void);
void i2c_start_read(uint8_t address, uint8_t length);

void *i2c_interface[] = {
	&i2c_stop,
	&i2c_write,
	&i2c_read,
	&i2c_configure,
	&i2c_start_read
};

LF_MODULE(i2c, "i2c", i2c_interface);

LF_WEAK void i2c_stop(void) {
	lf_invoke(lf_get_current_device(), "i2c", _i2c_stop, lf_void_t, NULL);
}

LF_WEAK void i2c_write(uint8_t byte) {
	lf_invoke(lf_get_current_device(), "i2c", _i2c_write, lf_void_t, lf_args(lf_infer(byte)));
}

LF_WEAK uint8_t i2c_read(void) {
	return lf_invoke(lf_get_current_device(), "i2c", _i2c_read, lf_void_t, NULL);
}

LF_WEAK int i2c_configure(void) {
	return lf_invoke(lf_get_current_device(), "i2c", _i2c_configure, lf_int32_t, NULL);
}

LF_WEAK void i2c_start_read(uint8_t address, uint8_t length) {
	lf_invoke(lf_get_current_device(), "i2c", _i2c_start_read, lf_void_t, lf_args(lf_infer(address), lf_infer(length)));
}

