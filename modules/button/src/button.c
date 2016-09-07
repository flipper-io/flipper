#define __private_include__
#include <flipper/button.h>

/* Define the virtual interface for this module. */
const struct _button button = {
	button_configure,
	button_read
};
