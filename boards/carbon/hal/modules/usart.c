#define __private_include__
#include <flipper/libflipper.h>
#include <flipper/carbon/usart.h>

int usart_configure(void) {
	lf_invoke(&_usart, _usart_configure, NULL);
	return lf_success;
}

void usart_enable(void) {
	lf_invoke(&_usart, _usart_enable, NULL);
}

void usart_disable(void) {
	lf_invoke(&_usart, _usart_disable, NULL);
}

uint8_t usart_ready(void) {
	return lf_invoke(&_usart, _usart_ready, NULL) >> 16;
}

void usart_put(uint8_t byte) {
	lf_invoke(&_usart, _usart_put, fmr_args(fmr_infer(byte)));
}

uint8_t usart_get(void) {
	return lf_invoke(&_usart, _usart_get, NULL) >> 16;
}

int usart_push(void *source, lf_size_t length) {
	return lf_push(&_usart, _usart_push, source, length, NULL);
}

int usart_pull(void *destination, lf_size_t length) {
	return lf_pull(&_usart, _usart_pull, destination, length, NULL);
}
