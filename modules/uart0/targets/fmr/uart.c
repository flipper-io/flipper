#define __private_include__
#include <flipper/flipper.h>
#include <flipper/modules.h>

/* ~ Provide the definition for this standard module. ~ */
struct _fmr_module _uart0 = {
	"uart0",
	"Provides low level access to the device's UART bus.",
	LF_VERSION,
	_uart0_id,
	NULL
};

void uart0_configure(void) {
	lf_invoke(&_uart0, _uart0_configure, NULL);
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
	lf_invoke(&_uart0, _uart0_put, fmr_args(fmr_int8(byte)));
}

uint8_t uart0_get(void) {
	return lf_invoke(&_uart0, _uart0_get, NULL) >> 16;
}

void uart0_push(void *source, uint32_t length) {
	lf_push(&_uart0, _uart0_push, source, length, NULL);
}

void uart0_pull(void *destination, uint32_t length) {
	lf_pull(&_uart0, _uart0_pull, destination, length, NULL);
}
