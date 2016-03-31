#define __private_include__
#include <flipper/at45.h>

/* ~ Define the virtual driver object. ~ */
const struct _at45 at45 = {

	at45_configure,
	at45_enable,
	at45_disable,
	at45_reset,
	at45_read,
	at45_alloc,
	at45_free,
	at45_format,
	at45_push,
	at45_pull,
	at45_dereference

};
