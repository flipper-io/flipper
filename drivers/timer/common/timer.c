#define __private_include__
#include <flipper/timer.h>

/* Define the virtual interface for this module. */
const struct _timer timer = {
	timer_configure
};
