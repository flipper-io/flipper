#define __private_include__
#include <flipper/carbon/timer.h>

#ifdef __use_timer__
/* Define the virtual interface for this module. */
const struct _timer timer = {
	timer_configure
};
#endif
