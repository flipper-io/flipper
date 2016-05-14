#define __private_include__
#include <flipper/wifi.h>

/* Define the virtual interface for this module. */
const struct _wifi wifi = {
	wifi_configure
};
