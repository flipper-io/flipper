#define __private_include__
#include <flipper/usart.h>

#ifdef __use_usart__

LF_MODULE(_usart, "usart", "Provides low level access to the device's UART bus.", NULL, NULL);

/* Define the virtual interface for this module. */
const struct _usart usart = {
	usart_configure,
	usart_ready,
	usart_push,
	usart_pull,
};

LF_WEAK int usart_configure(void) {
	lf_invoke(&_usart, _usart_configure, fmr_int_t, NULL);
	return lf_success;
}

LF_WEAK int usart_ready(void) {
	return lf_invoke(&_usart, _usart_ready, fmr_int_t, NULL);
}

LF_WEAK int usart_push(void *source, lf_size_t length) {
	return lf_push(&_usart, _usart_push, source, length, NULL);
}

LF_WEAK int usart_pull(void *destination, lf_size_t length) {
	return lf_pull(&_usart, _usart_pull, destination, length, NULL);
}

#endif
