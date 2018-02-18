#include <flipper.h>

#ifdef __use_temp__
#define __private_include__
#include <flipper/temp.h>

int temp_configure(void) {
	printf("Configuring the temperature sensor.\n");
	return lf_success;
}

#endif
