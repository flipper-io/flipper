#define __private_include__
#include <flipper/uart0.h>

#ifdef __use_uart0__

LF_MODULE(_uart0, "uart0", "Provides low level access to the device's UART bus.", NULL, NULL);

/* Define the virtual interface for this endpoint. */
const struct _uart0 uart0 = {
	uart0_configure,
	uart0_ready,
	uart0_push,
	uart0_pull
};

LF_WEAK int uart0_configure(uint8_t baud, uint8_t interrupts) {
	lf_invoke(&_uart0, _uart0_configure, fmr_int_t, fmr_args(fmr_infer(baud), fmr_infer(interrupts)));
	return lf_success;
}

LF_WEAK int uart0_ready(void) {
	return lf_invoke(&_uart0, _uart0_ready, fmr_int_t, NULL);
}

LF_WEAK int uart0_push(void *source, lf_size_t length) {
	return lf_push(&_uart0, _uart0_push, source, length, NULL);
}

LF_WEAK int uart0_pull(void *destination, lf_size_t length) {
	return lf_pull(&_uart0, _uart0_pull, destination, length, NULL);
}

#endif
