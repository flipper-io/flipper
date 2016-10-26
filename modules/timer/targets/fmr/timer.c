#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_timer, "timer", "Interfaces with the device's timer peripheral.", _timer_id);

int timer_configure(void) {
	return lf_invoke(&_timer, _timer_configure, NULL);
}
