#include <PACKAGE.h>

extern unsigned char app_bin[];
extern unsigned int app_bin_len;

LF_MODULE(_PACKAGE, "PACKAGE", "DESCRIPTION");

#ifdef __DEVICE__

const char _fmr_app_name[] __attribute__((section (".name"))) = "PACKAGE";

VARIABLES

const struct _PACKAGE PACKAGE __attribute__((section (".module"))) = {
STRUCT
};

#else

LF_WEAK int PACKAGE_configure(void) {
	int _e;
	suppress_errors(_e = lf_bind(&_app));
	if (_e == lf_error) {
		_e = lf_load(lf_get_current_device(), app_bin, app_bin_len);
		lf_assert(_e == lf_success, failure, E_MODULE, "Failed to load module '%s'.", _PACKAGE.name);
		_e = lf_bind(&_app);
		lf_assert(_e == lf_success, failure, E_MODULE, "Failed to bind module '%s'.", _PACKAGE.name);
	}
	return lf_success;
failure:
	return lf_error;
}

FUNCTIONS

#endif
