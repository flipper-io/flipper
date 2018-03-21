#include <flipper.h>

extern struct _lf_module temp;

int temp_configure(void) {
	dyld_register(&THIS_DEVICE, &temp);
	printf("Configured the temperature sensor.\n");
	return lf_success;
}
