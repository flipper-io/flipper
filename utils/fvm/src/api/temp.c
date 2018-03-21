#include <flipper.h>

LF_FUNC("temp") int temp_configure(void) {
	printf("Configured the temperature sensor.\n");
	return lf_success;
}
