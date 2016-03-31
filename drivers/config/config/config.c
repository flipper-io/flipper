#define __private_include__
#include <flipper/config.h>
#include <flipper/fmr.h>

void config_configure(void) {

}

void config_write(uint8_t key, uint16_t value) {

	device.invoke(_config, _config_write, 2, key, value);

}

uint16_t config_read(uint8_t key) {

	return device.invoke(_config, _config_read, 1, key);

}