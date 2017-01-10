#define __private_include__
#include <module/module.h>

#ifdef DEVICE_BUILD
#define MODULE_ATTRIBUTE __attribute__((section (".module")))
#else
#define MODULE_ATTRIBUTE
#endif

/* Define the virtual interface for this module. */
const struct _module module MODULE_ATTRIBUTE = {
	module_configure,
    module_test
};
