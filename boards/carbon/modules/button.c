#define __private_include__
#include <flipper/carbon/button.h>

#ifdef __use_button__
/* Define the virtual interface for this module. */
const struct _button button = {
	button_configure,
	button_read
};
#endif
