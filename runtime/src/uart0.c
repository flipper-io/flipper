#include <flipper/libflipper.h>

#ifdef __use_uart0__
#define __private_include__
#include <flipper/uart0.h>

LF_MODULE(_uart0, "uart0", "Provides low level access to the device's UART bus.");

/* Define the virtual interface for this module. */
const struct _lf_endpoint uart0 = {
	uart0_configure,
	uart0_ready,
	uart0_push,
	uart0_pull
};

LF_WEAK int uart0_configure(struct _lf_endpoint *self, void *_configuration) {
	struct _uart0_configuration *configuration = _configuration;
	lf_invoke(&_uart0, _endpoint_configure, NULL);
	return lf_success;
}

LF_WEAK bool uart0_ready(struct _lf_endpoint *self) {
	return lf_invoke(&_uart0, _endpoint_ready, NULL) >> 16;
}

LF_WEAK int uart0_push(struct _lf_endpoint *self, void *source, lf_size_t length) {
	return lf_push(&_uart0, _endpoint_push, source, length, NULL);
}

LF_WEAK int uart0_pull(struct _lf_endpoint *self, void *destination, lf_size_t length) {
	return lf_pull(&_uart0, _endpoint_pull, destination, length, fmr_args(fmr_int32(0)));
}

#pragma warning Move this.

LF_WEAK void uart0_dfu(struct _lf_endpoint *self) {
	lf_invoke(&_uart0, 0, NULL);
}

#endif
