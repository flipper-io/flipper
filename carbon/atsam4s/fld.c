#define __private_include__
#include <flipper/adc.h>
#include <flipper/atsam4s/atsam4s.h>
#include <loader.h>

int fld_configure(void) {
	return lf_success;
}

fmr_module fld_index(lf_crc_t identifier) {
    return os_get_module_index(identifier);
}
