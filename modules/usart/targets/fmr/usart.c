#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
LF_MODULE(_usart, "usart", "Provides low level access to the device's UART bus.", _usart_id);

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
	lf_invoke(&_usart, _usart_put, fmr_args(fmr_int8(byte)));
}

uint8_t usart_get(void) {
	return lf_invoke(&_usart, _usart_get, NULL) >> 16;
}

void usart_push(void *source, uint32_t length) {
	lf_push(&_usart, _usart_push, source, length, NULL);
}

void usart_pull(void *destination, uint32_t length) {
	lf_pull(&_usart, _usart_pull, destination, length, NULL);
}
