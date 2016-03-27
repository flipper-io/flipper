#define __private_include__
#include <flipper/sam/sam.h>

/* ~ Define the virtual driver object. ~ */
const struct _sam sam = {

	sam_configure,
	sam_set_power,
	sam_reset,
	sam_load_dfu,
	sam_format,
	sam_suspend,
	sam_engage

};
