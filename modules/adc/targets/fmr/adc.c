#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_adc, "adc", "Interfaces with the device's internal analog to digital converter.", _adc_id);

int adc_configure(void) {
	return lf_success;
}
