#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
struct _fmr_module _uart = {
	"uart",
	"Provides low level access to the device's UART bus.",
	LF_VERSION,
	_uart_id,
	NULL
};

void uart_configure(void) {
	lf_invoke(&_uart, _uart_configure, NULL);
}

void uart_enable(void) {
	lf_invoke(&_uart, _uart_enable, NULL);
}

void uart_disable(void) {
	lf_invoke(&_uart, _uart_disable, NULL);
}

uint8_t uart_ready(void) {
	return lf_invoke(&_uart, _uart_ready, NULL) >> 16;
}

void uart_put(uint8_t byte) {
	lf_invoke(&_uart, _uart_put, fmr_args(fmr_int8(byte)));
}

uint8_t uart_get(void) {
	return lf_invoke(&_uart, _uart_get, NULL) >> 16;
}

void uart_push(void *source, uint32_t length) {
	lf_push(&_uart, _uart_push, source, length);
}

void uart_pull(void *destination, uint32_t length) {
	lf_pull(&_uart, _uart_pull, destination, length);
}
