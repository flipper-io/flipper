#define __private_include__
#include <flipper/nvm.h>

/* Define the virtual interface for this module. */
const struct _nvm nvm = {
	nvm_configure,
	nvm_enable,
	nvm_disable,
	nvm_reset,
	nvm_read,
	nvm_get,
	nvm_alloc,
	nvm_free,
	nvm_format,
	nvm_push,
	nvm_pull,
	nvm_dereference
};
