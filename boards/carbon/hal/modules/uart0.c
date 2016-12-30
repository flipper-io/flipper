#define __private_include__
#include <flipper/flipper.h>
#include <flipper/carbon/uart0.h>

int uart0_configure(void) {
	lf_invoke(&_uart0, _uart0_configure, NULL);
	return lf_success;
}

void uart0_enable(void) {
	lf_invoke(&_uart0, _uart0_enable, NULL);
}

void uart0_disable(void) {
	lf_invoke(&_uart0, _uart0_disable, NULL);
}

uint8_t uart0_ready(void) {
	return lf_invoke(&_uart0, _uart0_ready, NULL) >> 16;
}

void uart0_put(uint8_t byte) {
	lf_invoke(&_uart0, _uart0_put, fmr_args(fmr_infer(byte)));
}

uint8_t uart0_get(void) {
	return lf_invoke(&_uart0, _uart0_get, NULL) >> 16;
}

int uart0_push(void *source, lf_size_t length) {
	return lf_push(&_uart0, _uart0_push, source, length, NULL);
}

int uart0_pull(void *destination, lf_size_t length) {
	return lf_pull(&_uart0, _uart0_pull, destination, length, NULL);
}
