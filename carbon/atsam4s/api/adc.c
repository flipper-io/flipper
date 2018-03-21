#include <flipper/adc.h>

extern struct _lf_module adc;

LF_FUNC("adc") int adc_configure(void) {
	return lf_success;
}
