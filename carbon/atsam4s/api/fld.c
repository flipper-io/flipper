#include <flipper/fld.h>
#include <os/loader.h>

int fld_configure(void) {
	return lf_success;
}

int fld_index(lf_crc_t identifier) {
	return os_get_module_index(identifier);
}
