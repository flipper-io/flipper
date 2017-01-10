#define __private_include__
#include <flipper/carbon/modules/uart0.h>

#ifdef __use_uart0__
/* Define the virtual interface for this module. */
const struct _uart0 uart0 = {
	uart0_configure,
	uart0_enable,
	uart0_disable,
	uart0_ready,
	uart0_put,
	uart0_get,
	uart0_push,
	uart0_pull,
};
#endif
