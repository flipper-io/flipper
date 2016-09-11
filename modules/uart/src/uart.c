#define __private_include__
#include <flipper/uart.h>

/* Define the virtual interface for this module. */
const struct _uart uart = {
	uart_configure,
	uart_enable,
	uart_disable,
	uart_ready,
	uart_put,
	uart_get,
	uart_push,
	uart_pull,
};
