#define __private_include__
#include <flipper/usart.h>

/* Define the virtual interface for this module. */
const struct _uart usart = {
	usart_configure,
	usart_enable,
	usart_disable,
	usart_ready,
	usart_put,
	usart_get,
	usart_push,
	usart_pull,
};
