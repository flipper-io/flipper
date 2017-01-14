#define __private_include__
#include <flipper/carbon/modules/fld.h>

#ifdef __use_fld__
/* Define the virtual interface for this module. */
const struct _fld fld = {
	fld_configure
};
#endif
