#define __private_include__
#include <flipper/config.h>
#include <flipper/fmr.h>

void config_configure(void) {

}

void config_write(lf_config key, uintconfig_t value) {

	device_invoke(_config, _config_read, device_args(key, value));

}

uintconfig_t config_read(lf_config key) {

	return (uintconfig_t)(device_invoke(_config, _config_read, device_args(key)));

}