#define __private_include__
#include <flipper/uart.h>

/* Define the virtual interface for this module. */
const struct _uart uart = {
	usart0_configure,
	usart0_enable,
	usart0_disable,
	usart0_ready,
	usart0_put,
	usart0_get,
	usart0_push,
	usart0_pull,
};
