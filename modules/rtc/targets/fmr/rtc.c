#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_rtc, "rtc", "Interfaces with the device's real time clock.", _rtc_id);

int rtc_configure(void) {
	return lf_success;
}
