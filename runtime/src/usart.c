#include <flipper/libflipper.h>

#ifdef __use_usart__
#define __private_include__
#include <flipper/usart.h>

LF_MODULE(_usart, "usart", "Provides low level access to the device's UART bus.");

/* Define the virtual interface for this module. */
const struct _usart usart = {
	usart_configure,
	usart_enable,
	usart_disable,
	usart_ready,
	usart_put,
	usart_get,
	usart_push,
	usart_pull,
};

LF_WEAK int usart_configure(void) {
	lf_invoke(&_usart, _usart_configure, NULL);
	return lf_success;
}

LF_WEAK void usart_enable(void) {
	lf_invoke(&_usart, _usart_enable, NULL);
}

LF_WEAK void usart_disable(void) {
	lf_invoke(&_usart, _usart_disable, NULL);
}

LF_WEAK uint8_t usart_ready(void) {
	return lf_invoke(&_usart, _usart_ready, NULL) >> 16;
}

LF_WEAK void usart_put(uint8_t byte) {
	lf_invoke(&_usart, _usart_put, fmr_args(fmr_infer(byte)));
}

LF_WEAK uint8_t usart_get(void) {
	return lf_invoke(&_usart, _usart_get, NULL) >> 16;
}

LF_WEAK int usart_push(void *source, lf_size_t length) {
	return lf_push(&_usart, _usart_push, source, length, NULL);
}

LF_WEAK int usart_pull(void *destination, lf_size_t length) {
	return lf_pull(&_usart, _usart_pull, destination, length, NULL);
}

#endif
