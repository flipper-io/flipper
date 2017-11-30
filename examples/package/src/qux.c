#define __private_include__
#include <qux/qux.h>

#ifdef DEVICE_BUILD
#define MODULE_ATTRIBUTE __attribute__((section (".module")))
#else
#define MODULE_ATTRIBUTE
#endif

/* Define the virtual interface for this module. */
const struct _qux qux MODULE_ATTRIBUTE = {
	qux_configure,
    qux_test
};
