#include <flipper/libflipper.h>

#ifdef __use_uart0__
#define __private_include__
#include <flipper/uart0.h>

LF_MODULE(_uart0, "uart0", "Provides low level access to the device's UART bus.");

/* Define the virtual interface for this module. */
const struct _uart0 uart0 = {
	uart0_configure,
	uart0_dfu,
	uart0_enable,
	uart0_disable,
	uart0_ready,
	uart0_put,
	uart0_get,
	uart0_push,
	uart0_pull,
};

LF_WEAK int uart0_configure(void) {
	lf_invoke(&_uart0, _uart0_configure, NULL);
	return lf_success;
}

LF_WEAK void uart0_dfu(void) {
	lf_invoke(&_uart0, _uart0_dfu, NULL);
}

LF_WEAK void uart0_enable(void) {
	lf_invoke(&_uart0, _uart0_enable, NULL);
}

LF_WEAK void uart0_disable(void) {
	lf_invoke(&_uart0, _uart0_disable, NULL);
}

LF_WEAK uint8_t uart0_ready(void) {
	return lf_invoke(&_uart0, _uart0_ready, NULL) >> 16;
}

LF_WEAK void uart0_put(uint8_t byte) {
	lf_invoke(&_uart0, _uart0_put, fmr_args(fmr_infer(byte)));
}

LF_WEAK uint8_t uart0_get(uint32_t timeout) {
	return lf_invoke(&_uart0, _uart0_get, fmr_args(fmr_infer(timeout))) >> 16;
}

LF_WEAK int uart0_push(void *source, lf_size_t length) {
	return lf_push(&_uart0, _uart0_push, source, length, NULL);
}

LF_WEAK int uart0_pull(void *destination, lf_size_t length, uint32_t timeout) {
	return lf_pull(&_uart0, _uart0_pull, destination, length, fmr_args(fmr_infer(timeout)));
}

#endif
