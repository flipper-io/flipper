#define __private_include__
#include <flipper/fld.h>

#ifdef __use_fld__

LF_MODULE(_fld, "fld", "Loads modules on the target device.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _fld fld = {
	fld_configure,
	fld_index
};

LF_WEAK int fld_configure(void) {
	return lf_invoke(&_fld, _fld_configure, fmr_int_t, NULL);
}

LF_WEAK int fld_index(lf_crc_t identifier) {
	return lf_invoke(&_fld, _fld_index, fmr_int_t, fmr_args(fmr_infer(identifier)));
}

#endif
